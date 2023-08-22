use poise::{serenity_prelude as ser, ReplyHandle};
use std::future::Future;
use std::time::Duration as StdDuration;
use std::pin::Pin;
use std::borrow::Cow;
use crate::{errors::{Result, Contextualizable}, PoiseContext};

pub fn edit<'a>(
	handle: &'a ReplyHandle,
	ctx: PoiseContext<'a>,
	content: impl Into<String> + Send + 'a
) -> impl FnOnce(ser::CreateActionRow) -> Pin<Box<dyn Future<Output = Result<Cow<'a, ser::Message>>> + Send + 'a>> {
	move |ar| Box::pin(async move {
		handle.edit(ctx, |e| e.content(content).components(|c| c.add_action_row(ar))).await?;
		Ok(handle.message().await?)
	})
}

pub async fn ask_yn<F, Fut, M>(ctx: impl AsRef<ser::ShardMessenger>, sender: F) -> Result<Option<bool>>
where F: FnOnce(ser::CreateActionRow) -> Fut,
	  Fut: Future<Output = Result<M>> + Send,
	  M: AsRef<ser::Message> {
	let msg = sender({
		let mut ar = ser::CreateActionRow::default();
		ar.create_button(|b| {
			b.label("Yes").custom_id("yes")
				.label("No").custom_id("no")
		});
		ar
	}).await?;
	let interaction = msg.as_ref().await_component_interaction(ctx.as_ref())
		.author_id(msg.as_ref().author.id)
		.timeout(StdDuration::from_secs(60 * 10))
		.filter(|m| ["yes", "no"].contains(&m.data.custom_id.as_str()))
		.await;
	
	match interaction {
		Some(m) if m.data.custom_id == "yes" => Ok(Some(true)),
		Some(m) if m.data.custom_id == "no"  => Ok(Some(false)),
		_ => Ok(None)
	}
}

