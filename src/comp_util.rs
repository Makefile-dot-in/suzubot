use poise::{serenity_prelude as ser, ReplyHandle};
use std::future::Future;
use std::time::Duration as StdDuration;
use std::pin::Pin;
use std::borrow::Cow;
use crate::{errors::Result, PoiseContext, cmd_data};

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

