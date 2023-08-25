use std::{collections::HashMap, borrow::Cow};
use std::time::Duration as StdDuration;
use std::fmt::{self, Display};
use poise::serenity_prelude::{self as ser};
use ser::{Mentionable, ExecuteWebhook};
use tokio::{sync::{RwLock, Mutex}, time::Instant};
use async_trait::async_trait;
use crate::errors::{Error, Result, Contextualizable};

#[derive(Debug, Clone)]
pub enum WebhookErrorContext {
	GettingWebhooks(ser::ChannelId),
	CreatingWebhook(ser::ChannelId),
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

