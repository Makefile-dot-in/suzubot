use futures::StreamExt;
use itertools::Itertools;
use poise::{serenity_prelude as ser, ReplyHandle, ChoiceParameter};
use std::{future::Future, fmt::Display};
use std::time::Duration as StdDuration;
use std::pin::Pin;
use std::fmt::Debug;
use std::borrow::Cow;
use std::fmt;
use crate::{errors::Result, PoiseContext, cmd_data};

#[derive(ChoiceParameter, Clone, Copy)]
enum CacheType {
    User,
    Message,
    Channel,
    Guild,
}

impl CacheType {
    fn get_debug_from_cache(
        self,
        c: impl AsRef<ser::Cache>,
        id: u64,
        cid: ser::ChannelId
    ) -> Option<String> {
        fn b<D: Debug>(d: D) -> String { format!("{d:#?}") }
        match self {
            Self::User => c.as_ref().user(ser::UserId(id)).map(b),
            Self::Message => c.as_ref().message(cid, ser::MessageId(id)).map(b),
            Self::Channel => c.as_ref().channel(ser::ChannelId(id)).map(b),
            Self::Guild => c.as_ref().guild(ser::GuildId(id)).map(b),
        }
    }
}

#[poise::command(slash_command, owners_only, custom_data = "cmd_data().test_mode()")]
pub async fn cache_test(
    ctx: PoiseContext<'_>,
    typ: CacheType,
    id: String,
    cid: Option<ser::ChannelId>) -> Result<()> {
    let idp = match id.parse::<u64>() {
        Ok(c) => c,
        Err(e) => {
            ctx.say(format!("{e:?}")).await?;
            return Ok(())
        }
    };
    match typ.get_debug_from_cache(ctx, idp, cid.unwrap_or(ser::ChannelId(0))) {
        Some(item) => {
            ctx.send(|r| {
                r.attachment(ser::AttachmentType::Bytes {
                    data: item.into_bytes().into(),
                    filename: "cache_item.txt".to_owned()
                })
            }).await?;
        },
        None => {
            ctx.say("no item found").await?;
        }
    }
    Ok(())
}

#[poise::command(slash_command, owners_only, custom_data = "cmd_data().test_mode()")]
pub async fn component_test(ctx: PoiseContext<'_>) -> Result<()> {
    let handle = ctx.send(|m| {
        m.content("component test").components(|c| {
            c.create_action_row(|ar| {
                ar.create_button(|b| b.label("Yes").custom_id("yes"))
                  .create_button(|b| b.label("No").custom_id("no"))
            })
        })
    }).await?;
    let interaction = handle.message().await?
                            .await_component_interaction(ctx)
                            .await;


    match interaction {
        Some(int) => {
            /*int.create_interaction_response(ctx, |r| {
            r.kind(ser::InteractionResponseType::UpdateMessage)
            .interaction_response_data(|d| d.components(|c| c).content("Meow"))
        }).await?;*/
            int.defer(ctx).await?;
        },
        None => {
            log::warn!("no interaction received");
        }
    }

    handle.edit(ctx, |b| b.components(|c| c).content("indirectly through ctx")).await?;
    log::info!("got this far");
    Ok(())
}

pub fn edit<'a>(
    handle: &'a ReplyHandle,
    ctx: PoiseContext<'a>,
    content: impl Into<String> + Send + 'a
) -> impl FnOnce(ser::CreateActionRow)
                 -> Pin<Box<dyn Future<Output = Result<Cow<'a, ser::Message>>> + Send + 'a>> {
    move |ar| Box::pin(async move {
        handle.edit(ctx, |e| e.content(content).components(|c| c.add_action_row(ar))).await?;
        Ok(handle.message().await?)
    })
}


pub async fn ask_yn<F, Fut, M>(
    ctx: impl AsRef<ser::ShardMessenger> + AsRef<ser::Http> + Copy,
    user: impl Into<ser::UserId>,
    sender: F
) -> Result<Option<bool>>
where F: FnOnce(ser::CreateActionRow) -> Fut,
      Fut: Future<Output = Result<M>> + Send,
      M: AsRef<ser::Message> {
    let user = user.into();
    let msg = sender({
        let mut ar = ser::CreateActionRow::default();
        ar.create_button(|b| b.label("Yes").custom_id("yes"))
          .create_button(|b| b.label("No").custom_id("no"));
        ar
    }).await?;
    let interaction = msg.as_ref().await_component_interaction(ctx)
                                  .author_id(user)
                                  .timeout(StdDuration::from_secs(60 * 10))
                                  .filter(|m| ["yes", "no"].contains(&m.data.custom_id.as_str()))
                                  .await;
    log::debug!("{interaction:#?}");
    if let Some(m) = &interaction { m.defer(ctx).await?; }
    match interaction {
        Some(m) if m.data.custom_id == "yes" => Ok(Some(true)),
        Some(m) if m.data.custom_id == "no"  => Ok(Some(false)),
        _ => Ok(None)
    }
}

/// a trait for elements that can be listed in a discord markdown list
pub trait ListElementModel {
    fn pagination_fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}


pub struct ListedElement<T: ListElementModel>(T);

impl<T: ListElementModel> Display for ListedElement<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.pagination_fmt(f)
    }
}

pub async fn paginate<Item, ItemGen, ItemGenFut, ItemGenIt>(
    ctx: PoiseContext<'_>,
    user: impl Into<ser::UserId>,
    title: impl ToString + Display,
    mut generator: ItemGen,
) -> Result<()>
where Item: ListElementModel,
      ItemGen: FnMut(u32) -> ItemGenFut,
      ItemGenFut: Future<Output = Result<(ItemGenIt, u32, u32)>>,
      ItemGenIt: IntoIterator<Item = Item> {
    let mut ar = ser::CreateActionRow::default();
    ar.create_button(|b| b.emoji('◀').custom_id("prev"))
      .create_button(|b| b.emoji('▶').custom_id("next"));

    fn embed<Item: ListElementModel>(
        title: impl ToString + Display,
        items: impl IntoIterator<Item = Item>,
        page: u32,
        total: u32
    ) -> ser::CreateEmbed {
        let mut ce = ser::CreateEmbed::default();
        ce.title(format!("{title} (Page {page_p1} of {total})", page_p1 = page + 1))
          .description({
              items
                  .into_iter()
                  .map(ListedElement)
                  .map(|el| format!("* {el}"))
                  .join("\n")
          });
        ce
    }

    let mut pagenum = 0;
    let (items, total, newpagenum) = generator(pagenum)
        .await
        .inspect_err(|err| log::error!("{err}"))?;
    pagenum = newpagenum;
    let replyh = ctx.send(|m| {
       m.embed(|e| { *e = embed(&title, items, pagenum, total); e })
        .components(|c| c.add_action_row(ar))
    }).await?;

    let mut interactions = replyh
        .message()
        .await?
        .await_component_interactions(ctx)
        .author_id(user.into())
        .timeout(StdDuration::from_secs(60 * 5))
        .filter(|m| ["next", "prev"].contains(&m.data.custom_id.as_str()))
        .build();

    while let Some(interaction) = interactions.next().await {
        match interaction.data.custom_id.as_str() {
            "next" => pagenum = pagenum.saturating_add(1),
            "prev" => pagenum = pagenum.saturating_sub(1),
            _ => continue,
        }

        let Ok((items, total, newpagenum)) = generator(pagenum)
            .await
            .inspect_err(|err| log::error!("{err}"))
        else { continue };
        pagenum = newpagenum;

        interaction.create_interaction_response(ctx, |ir| {
            ir.kind(ser::InteractionResponseType::UpdateMessage)
              .interaction_response_data(|cird| {
                  cird.set_embed(embed(&title, items, pagenum, total))
              })
        }).await?;
    }
    Ok(())
}
