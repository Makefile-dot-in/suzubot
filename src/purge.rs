use crate::comp_util::{ask_yn, edit};
use crate::errors::{Contextualizable, Error, AsyncReportErr, Result, LogError};
use crate::log::{LogErrorContext, LogType};
use crate::cmd_data;

use super::PoiseContext;

use chrono::{Duration, Utc};
use futures::{future, Stream, StreamExt, TryStreamExt, TryFutureExt};
use itertools::Itertools;
use poise::ReplyHandle;
use poise::serenity_prelude::{self as ser};
use ser::Mentionable;
use regex::Regex;

use std::fmt::Display;

use std::vec;


use std::future::Future;
use std::fmt;


const ASK_FOR_CONFIRMATION_ABOVE: usize = 300;

#[derive(Debug)]
pub enum PurgeErrorContext {
   Deleting(ser::ChannelId),
}

impl Display for PurgeErrorContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
      match self {
         Self::Deleting(chid) =>
            write!(f, "deleting messages in {chid_mention}",
                  chid_mention = chid.mention())
      }
    }
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
        .try_take_while(|msg| future::ready(Ok(!after.as_ref().is_some_and(|a| a.id >= msg.id))))
        .try_filter(|msg| future::ready(!author.as_ref().is_some_and(|a| a != &msg.author)))
        .try_filter(|msg| {
            future::ready(!re.as_ref().is_some_and(|p| !p.is_match(&msg.content)))
        })
        .take(limit.into())
        .try_collect::<Vec<_>>()
        .await?;

    if messages.len() >= ASK_FOR_CONFIRMATION_ABOVE {
        let response = ask_yn(
            ctx,
            ctx.author(),
            edit(
                &handle,
                ctx,
                format!("This will delete **{}** messages. Are you sure?", messages.len())
            )
        ).await?;
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

    let delfut = delete_messages(ctx, ctx.channel_id(), messageids)
        .map_err(|e| e.contextualize(PurgeErrorContext::Deleting(ctx.channel_id())));
    let logfut = crate::log::log_purge(
        ctx,
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


    if sequential {
        sequential_purge(ctx, &handle, logfut, delfut).await?;
    } else {
        concurrent_purge(ctx, logfut, delfut).await?;
    };


    handle.edit(ctx, |e| e.content("Purge successful!")).await?;
    Ok(())
}

fn first_and_last<T: Copy>(slice: &[T]) -> Option<(T, T)> {
   slice.first()
      .cloned()
      .zip(slice.last().cloned())
}

/// deletes `messages`. `messages` must be ordered in descending order by time of creation.
async fn delete_messages(
   ctx: PoiseContext<'_>,
   channelid: ser::ChannelId,
   messages: impl IntoIterator<Item = ser::MessageId>,
) -> Result<()> {
   let msg_guard = ctx.data().logdata.monopolize(messages);
   let mut errors: Vec<(Option<(ser::MessageId, ser::MessageId)>, Error)> = Vec::new();
   let mut iter = msg_guard.items().map(Clone::clone);
   let min_ts: ser::Timestamp = (Utc::now() - Duration::days(7)).into();
   let mut arrch = iter.by_ref().take_while(|m| m.created_at() > min_ts).array_chunks::<100>();
   for msgs in arrch.by_ref() {
      channelid.delete_messages(ctx, msgs).await
         .map_err(|e| errors.push((first_and_last(&msgs), e.into())))
         .ok();
   }

   if let Some(remainder) = arrch.into_remainder() {
      if remainder.len() >= 2 {
         let remainder_slice = remainder.as_slice();
         channelid.delete_messages(ctx, remainder_slice.iter().map(Clone::clone)).await
            .map_err(|e| errors.push((first_and_last(remainder_slice), e.into()))).ok();
      } else {
         for msg in remainder {
            channelid.delete_message(ctx, msg).await
               .map_err(|e| errors.push((Some((msg, msg)), e.into()))).ok();
         }
      }
   }

   for msg in iter {
      channelid.delete_message(ctx, msg).await
         .map_err(|e| errors.push((Some((msg, msg)), e.into()))).ok();
   }

   if !errors.is_empty() {
      let errorstr = errors.into_iter().map(|(message_range, err)| {
         match message_range {
            Some((m1, m2)) if m1 == m2 =>
               format!("error purging {m1_link}: {err}",
                     m1_link = m1.link(channelid, ctx.guild_id())),
            Some((m1, m2)) =>
               format!("error purging {m1_link} to {m2_link}: {err}",
                     m1_link = m1.link(channelid, ctx.guild_id()),
                     m2_link = m2.link(channelid, ctx.guild_id())),
            None =>
               format!("error purging {err}"),
         }
      }).join("\n");

      if errorstr.len() <= 1500 {
         ctx.say(format!("Encountered errors while deleting: {errorstr}")).await?;
      } else {
         ctx.send(move |m| {
            m.content("Encountered errors while deleting:")
               .attachment(ser::AttachmentType::Bytes {
                  data: errorstr.into_bytes().into(),
                  filename: "errors.txt".to_owned()
               })
         }).await?;
      }
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
         ctx.author(),
         edit(handle, ctx, format!("⚠️ **WARNING:** logging failed: {logerr}. \
                                    The purged messages will be **irrevocably** lost.\
                                    Proceed anyway?"))
      ).await?;
      match response {
         Some(true) => (),
         _ => {
            handle.edit(ctx, |e| e.content("Purge cancelled.").components(|c| c)).await?;
            return Ok(());
         }
      }
   }
   deletion.await?;
   Ok(())
}

async fn concurrent_purge(
   ctx: PoiseContext<'_>,
   log: impl Future<Output = Result<()>> + Send,
   deletion: impl Future<Output = Result<()>> + Send
) -> Result<()> {
   let logfut_with_err = async move {
      log.await.report_err(|err| ctx.say(err)).await.logerr();
      Ok(())
   };
   futures::try_join!(logfut_with_err, deletion)?;

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

#[poise::command(slash_command, owners_only, custom_data = "cmd_data().test_mode()")]
pub async fn message_stream_test(
   ctx: PoiseContext<'_>,
   limit: u16,
   before: Option<ser::Message>
) -> Result<()> {
   ctx.defer().await?;
   let suzu_msgs = MessageStream::new(ctx, ctx.channel_id(), before.as_ref())
      .take(limit.into())
      .try_collect::<Vec<_>>().await?;
   let ser_msgs = ctx.channel_id()
      .messages_iter(ctx)
      .try_take_while(|m| future::ready(Ok(!before.as_ref().is_some_and(|b| m.id >= b.id))))
      .take(limit.into())
      .try_collect::<Vec<_>>().await?;

   for (suzu_msg, ser_msg) in suzu_msgs.into_iter().zip(ser_msgs.into_iter()) {
      if suzu_msg.id != ser_msg.id {
         ctx.say(format!("Stream mismatch ({suzu_msg_id} != {ser_msg_id})",
                         suzu_msg_id = suzu_msg.id,
                         ser_msg_id = ser_msg.id)).await?;
         return Ok(());
      }
   }

   ctx.say("Test completed successfully").await?;
   Ok(())
}
