use std::{collections::HashMap, borrow::Cow};
use std::time::Duration as StdDuration;
use std::fmt::{self, Display};
use poise::serenity_prelude::{self as ser};
use reqwest::Response;
use ser::{Mentionable, ExecuteWebhook};
use tokio::{sync::{RwLock, Mutex}, time::Instant};
use async_trait::async_trait;
use crate::errors::{Error, Result, Contextualizable};

#[derive(Debug, Clone)]
pub enum WebhookErrorContext {
	GettingWebhooks(ser::ChannelId),
	CreatingWebhook(ser::ChannelId),
	ExecutingWebhook(ser::WebhookId),
}

impl Display for WebhookErrorContext {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use WebhookErrorContext::*;
		match self {
			GettingWebhooks(channel) =>
				write!(f, "getting webhooks for channel {channel_mention}",
					   channel_mention = channel.mention()),
			CreatingWebhook(channel) =>
				write!(f, "creating a webhook in channel {channel_mention}",
					   channel_mention = channel.mention()),
			ExecutingWebhook(webhook) =>
				write!(f, "executing webhook {webhook}")
		}
	}
}

/// checks whether a channel has a webhook with a particular name; if yes, returns it, if no, creates one and returns it.
pub async fn create_or_return_webhook_for_channel(
	ctx: impl AsRef<ser::Http> + Copy,
	channel: impl Into<ser::ChannelId>,
	name: &str
) -> Result<ser::Webhook> {
	let channel = channel.into();
	let created_webhook = channel.webhooks(ctx)
		.await
		.map_err(Error::from)
		.contextualize(WebhookErrorContext::CreatingWebhook(channel))?
		.into_iter()
		.find(|wh| wh.name.as_ref().is_some_and(|n| &*n == name));

	match created_webhook {
		Some(wh) => Ok(wh),
		None => channel.create_webhook(ctx, name).await
			.map_err(Error::from)
			.contextualize(WebhookErrorContext::GettingWebhooks(channel))
	}
}

/// overcomplicated way of specifying the `wait` parameter
#[async_trait]
pub trait WebhookExecutionResult: Sized {
	fn wait() -> bool;
	async fn from_response(re: reqwest::Response) -> ser::Result<Self>;
}

#[async_trait]
impl WebhookExecutionResult for () {
	fn wait() -> bool { false }
	async fn from_response(_re: reqwest::Response) -> ser::Result<Self> {
		Ok(())
	}
}

#[async_trait]
impl WebhookExecutionResult for ser::Message {
	fn wait() -> bool { true }

	async fn from_response(re: reqwest::Response) -> ser::Result<Self> {
		re.json::<ser::Message>().await.map_err(Into::into)
	}
}

/// webhook executor that handles thread id which serenity won't
/// won't be needed when serenity hits 0.12
#[derive(Debug)]
pub struct WebhookExecutor {
    client: reqwest::Client,
    routes: RwLock<HashMap<ser::WebhookId, Mutex<Instant>>>,
}

impl WebhookExecutor {
    pub fn new() -> Self {
        Self {
            client: reqwest::Client::builder()
                .user_agent("suzu custom webhook thing because the serenity one isn't very good")
                .build()
                .unwrap(),
            routes: RwLock::new(HashMap::new()),
        }
    }

    async fn file_to_part(file: tokio::fs::File) -> reqwest::multipart::Part {
        match file.metadata().await {
            Ok(m) => reqwest::multipart::Part::stream_with_length(file, m.len()),
            Err(_) => reqwest::multipart::Part::stream(file),
        }
    }

    pub async fn execute<F, R>(
        &self,
        webhook: &ser::Webhook,
        thread_id: Option<impl Into<ser::ChannelId>>,
        builder: F,
    ) -> ser::Result<R>
    where
        F: for<'a, 'b> FnOnce(&'a mut ExecuteWebhook<'b>) -> &'a mut ExecuteWebhook<'b>,
		R: WebhookExecutionResult
    {	
		let mut senderr: Option<reqwest::Error> = None;
		let mut exwhb: ExecuteWebhook = Default::default();
		let thread_id = thread_id.map(Into::into);
		builder(&mut exwhb);
		
		let ExecuteWebhook(jsonmap, attachments) = exwhb;
		for i in 2..=4 {
			let mut url = reqwest::Url::parse("https://discord.com/api/v10/webhooks").unwrap();
			url.path_segments_mut()
				.unwrap()
				.push(&webhook.id.to_string())
				.push(&webhook.token.as_ref().ok_or(ser::ModelError::NoTokenSet)?);
			let mut form = reqwest::multipart::Form::new()
				.text("payload_json", ser::json::prelude::to_string(&jsonmap)?);
			
			for (idx, attachment) in attachments.iter().enumerate() {
				let part = match attachment {
					// this api is the worst ever conceived by humanity. like wtf is this. bestie just fucking let
					// me pass in a stream or a fucking Vec<u8> since this shit gets copied anyway
					ser::AttachmentType::Bytes { data, filename } => {
						reqwest::multipart::Part::bytes(data.clone()).file_name(filename.clone())
					}
					ser::AttachmentType::File { file, filename } => {
						let _ = file.sync_all().await;			
						WebhookExecutor::file_to_part(file.try_clone().await?)
							.await
							.file_name(filename.clone())
					}
					ser::AttachmentType::Path(path) => {
						let mut part =
							WebhookExecutor::file_to_part(tokio::fs::File::open(path).await?).await;
						if let Some(filename) = path.file_name() {
							part = part.file_name(filename.to_string_lossy());
						}
						part
					}
					ser::AttachmentType::Image(url) => {
						let response = self.client.get(url.clone()).send().await?;
						let mut part = match response.content_length() {
							Some(len) => reqwest::multipart::Part::stream_with_length(response, len),
							None => reqwest::multipart::Part::stream(response),
						};

						if let Some(filename) = url.path_segments().and_then(Iterator::last) {
							part = part.file_name(filename.to_owned());
						}

						part
					}
					_ => continue,
				};
				form = form.part(format!("file[{idx}]"), part);
			}

			// Cow<str> because we have a mixture of owned and non-owned strings
			let mut query: Vec<(Cow<str>, Cow<str>)> = vec![("wait".into(), if R::wait() { "true".into() } else { "false".into() })];
			if let Some(thrid) = thread_id {
				query.push(("thread_id".into(), thrid.0.to_string().into()));
			}

			let mut map_guard = self.routes.read().await;
			let duration_mutex = match map_guard.get(&webhook.id) {
				Some(mutex) => mutex,
				None => {
					drop(map_guard);
					let mut write_guard = self.routes.write().await;
					write_guard.insert(webhook.id, Mutex::new(Instant::now()));
					drop(write_guard);
					map_guard = self.routes.read().await;
					&map_guard[&webhook.id]
				}
			};
			let mut duration_guard = duration_mutex.lock().await;
			// everything has been processed, time to sleep
			tokio::time::sleep_until(*duration_guard).await;
			
			let reres: reqwest::Result<Response> = try {
				self
					.client
					.post(url)
					.query(&query)
					.multipart(form)
					.send()
					.await?
					.error_for_status()?
			};

			let re = match reres {
				Ok(r) => r,
				Err(e) => {
					if i < 4 {
						log::error!("{e}, resending... (try {i})");
					} else {
						log::error!("{e}, giving up.");
					}
					
					tokio::time::sleep(StdDuration::from_secs(60)).await;
					senderr = Some(e);
					continue
				}
			};

			let reset_after = re
				.headers()
				.get("X-RateLimit-Reset-After")
				.ok_or(ser::HttpError::RateLimitUtf8)? // serenity doesn't have a better error
				.to_str()
				.map_err(|_| ser::HttpError::RateLimitUtf8)?
				.parse::<f64>()
				.map_err(|_| ser::HttpError::RateLimitI64F64)?;
			
			let reset_at = Instant::now() + StdDuration::from_secs_f64(reset_after);
			*duration_guard = reset_at;

			return Ok(R::from_response(re).await?);
		}

		Err(senderr.unwrap().into())
	}
}
