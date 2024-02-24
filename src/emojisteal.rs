use crate::ser;
use crate::Error;
use crate::{errors::Result, PoiseContext};
use base64::Engine;
use lazy_static::lazy_static;
use regex::Regex;
use base64::engine::general_purpose::STANDARD as BASE64_ENGINE;

lazy_static! {
    static ref EMOJI_REGEX: Regex = Regex::new(r"<a?:[\w_]+:\d+>").unwrap();
}

#[poise::command(
    slash_command,
    guild_only = true,
    default_member_permissions = "MANAGE_EMOJIS_AND_STICKERS"
)]
pub async fn emojisteal(
    ctx: PoiseContext<'_>,
    #[max_length = 1000]
    #[description = "Emojis to steal"]
    emojis: String,
) -> Result<()> {
    let guild_id = ctx.guild_id().unwrap();
    let emojis = EMOJI_REGEX
        .find_iter(&emojis)
        .filter_map(|e| ser::parse_emoji(e.as_str()));
    let client = reqwest::Client::new();
    for emoji in emojis {
        let Ok(res) = client.get(&emoji.url())
            .send()
            .await
        else { continue };
        let Some(mime) = res.headers().get("Content-Type")
            .and_then(|x| x.to_str().ok())
            .map(str::to_owned)
        else { continue };
        let Ok(bytes) = res.bytes().await else { continue };
        let b64 = BASE64_ENGINE.encode(bytes);
        let emoji_data = format!("data:{mime};base64,{b64}");
        guild_id.create_emoji(ctx, &emoji.name, &emoji_data).await?;
    }

    ctx.say("Emojis added").await?;
    Ok(())
}
