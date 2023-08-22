use crate::comp_util::{ask_yn, edit};
use crate::errors::{CmdResult, Contextualizable, Error, OptError, InternalError, WithContext, AsyncReportErr, Result};
use crate::log::{LogErrorContext, LogType};

use super::PoiseContext;

use chrono::{Duration, Utc};
use futures::future;
use futures::{Stream, StreamExt, TryStream, TryStreamExt, TryFutureExt};
use poise::ReplyHandle;
use poise::serenity_prelude::{self as ser, ExecuteWebhook};
use regex::Regex;
use reqwest::header::HeaderMap;
use std::time::Duration as StdDuration;
use std::{collections::HashMap, ops::Index, vec};
use tokio::{
    sync::{Mutex, MutexGuard, RwLock, RwLockReadGuard},
    time::Instant,
};
use std::future::Future;

const ASK_FOR_CONFIRMATION_ABOVE: usize = 300;

pub struct PurgeErrorContext {
	
}

/// Purges messages.
#[poise::command(slash_command, guild_only = true, default_member_permissions = "MANAGE_MESSAGES")]
pub async fn purge(
    ctx: PoiseContext<'_>,
    #[description = "Purge no more than this many messages."]
    #[max = 10000]
    limit: u16,
    #[description = "Purge no more than this many messages back."]
    #[max = 20000]
    inlast: Option<u16>,
    #[description = "Purge only messages by this author"] author: Option<ser::User>,
    #[description = "Only purge messages matching this regex"]
    #[max_length = 100]
    pattern: Option<String>,
    #[description = "Start purging above this message."] before: Option<ser::Message>,
    #[description = "Stop purging below this message."] after: Option<ser::Message>,
    #[description = "Reason (shows up in log)"]
    #[max_length = 100]
    reason: Option<String>,
	#[description = "Whether to wait until logging finishes before purging. Defeaults to false"]
	sequential: Option<bool>
) -> Result<()> {
    let handle = ctx
        .send(|r| r.content("Purging messages...").ephemeral(true))
        .await?;
    let inlast = inlast.unwrap_or(20000);
    let re = match pattern.as_ref().map(|x| Regex::new(x)) {
        Some(Ok(re)) => Some(re),
        None => None,
        Some(Err(err)) => {
            handle
                .edit(ctx, |e| e.content(format!("Error compiling regex: {err}")))
                .await?;
            return Ok(());
        }
    };
	let sequential = sequential.unwrap_or(false);

    if before.as_ref().zip(after.as_ref())
        .is_some_and(|(a, b)| a.channel_id != ctx.channel_id() || b.channel_id != ctx.channel_id())
    {
        handle
            .edit(ctx, |e| {
                e.content("`after` and `before` must be from the channel where purge is run")
            })
            .await?;
        return Ok(());
    }

    let channelid = before
        .as_ref()
        .map(|m| m.channel_id)
        .unwrap_or(ctx.channel_id());
    let messages = MessageStream::new(ctx, channelid, before.as_ref().map(|b| b.id))
        .take(inlast.into())
        .try_take_while(|msg| future::ready(Ok(after.as_ref().is_some_and(|a| a.id < msg.id))))
        .try_filter(|msg| future::ready(!author.as_ref().is_some_and(|a| a != &msg.author)))
        .try_filter(|msg| {
            future::ready(!re.as_ref().is_some_and(|p| !p.is_match(&msg.content)))
        })
        .take(limit.into())
        .try_collect::<Vec<_>>()
		.await?;
	
    if messages.len() >= ASK_FOR_CONFIRMATION_ABOVE {
		let response = ask_yn(ctx, edit(&handle, ctx, format!("This will delete **{}** messages. Are you sure?", messages.len()))).await?;
        match response {
            Some(true) => (),
            _ => {
                handle.edit(ctx, |e| e.content("Purge cancelled.").components(|c| c)).await?;
                return Ok(());
            }
        }
    }
	
    handle.edit(ctx, |e| e.content("Purging...").components(|c| c)).await?;

	let messageids = messages.iter().map(|m| m.id).collect::<Vec<_>>();
	let logfut = crate::log::log_purge(
		&ctx,
		ctx.guild_id().unwrap(),
		limit,
		messages.len()
			.try_into()
			.unwrap(),
		inlast,
		author.map(|a| a.id),
		before.map(|b| b.id),
		after.map(|a| a.id),
		pattern,
		reason,
		messages.into_iter().rev()
	).map_err(|e| e.contextualize(LogErrorContext::Log(ctx.guild_id().unwrap(), LogType::Purge)));
	
    Ok(())
}

/// deletes `messages`. `messages` must be ordered in descending order by time of creation.
async fn delete_messages(
	client: impl AsRef<ser::Http>,
	channelid: ser::ChannelId,
	messages: impl IntoIterator<Item = ser::MessageId>
) -> Result<()> {
	let mut iter = messages.into_iter();
	let min_ts: ser::Timestamp = (Utc::now() - Duration::days(7)).into();
	let mut arrch = iter.by_ref().take_while(|m| m.created_at() > min_ts).array_chunks::<100>();
	for msgs in arrch.by_ref() {
		channelid.delete_messages(client.as_ref(), msgs).await?;
	}
	
	Ok(())
}

async fn sequential_purge(
	ctx: PoiseContext<'_>,
	handle: &ReplyHandle<'_>,
	log: impl Future<Output = Result<()>> + Send,
	deletion: impl Future<Output = Result<()>> + Send
) -> Result<()> {
	let logres = log.await;
	if let Err(logerr) = logres {
		let response = ask_yn(
			ctx,
			edit(handle, ctx, format!("⚠️ **WARNING:** logging failed: {logerr}. The purged messages will be **irrevocably** lost.\
									   Proceed anyway?"))
		).await?;
		match response {
			Some(true) => (),
			_ => {
				handle.edit(ctx, |e| e.content("Purge cancelled.").components(|c| c)).await;
				return Ok(());
			}
		}
	}
	deletion.await?;
	Ok(())
}

async fn concurrent_purge(
	log: impl Future<Output = Result<()>> + Send,
	deletion: impl Future<Output = Result<()>> + Send
) -> Result<()> {
	futures::try_join!(log, deletion)?;
	Ok(())
}

/// stream of serenity messages that may start from a certain message
/// (serenity's [`ChannelId::messages_iter`](ser::ChannelId::messages_iter) forces to start at the latest message)
struct MessageStream<H: AsRef<ser::Http>> {
    client: H,
    channel: ser::ChannelId,
    before: Option<ser::MessageId>,
    buffer: vec::IntoIter<ser::Message>,
}

impl<H: AsRef<ser::Http>> MessageStream<H> {
    fn new(
        client: H,
        channel: ser::ChannelId,
        before: Option<impl Into<ser::MessageId>>,
    ) -> impl Stream<Item = Result<ser::Message>> {
        let before = before.map(|m| m.into());
        let init = MessageStream {
            client,
            channel,
            before,
            buffer: Vec::new().into_iter(),
        };
        futures::stream::try_unfold(init, |mut state| async move {
            if state.buffer.len() == 0 {
                state.buffer = state
                    .channel
                    .messages(&state.client, |m| {
                        if let Some(msgid) = state.before {
                            m.before(msgid);
                        }
                        m.limit(100)
                    })
                    .await?
                    .into_iter();
            }
            let next = state.buffer.next();
            if let Some(message) = next.as_ref() {
                state.before = Some(message.id);
            }
            Ok(next.map(move |msg| (msg, state)))
        })
    }
}
