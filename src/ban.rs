use std::borrow::Cow;
use std::fmt::{self, Display};

use crate::PoiseContext;
use lazy_static::lazy_static;
use regex::{Captures, Regex};
use crate::ser;
use ser::Mentionable;
use crate::linkable::Linkable;
use crate::errors::Contextualizable;
use crate::errors::Result;

struct BanSettings {
    pub template: String
}

impl BanSettings {
    const LOOKUP_COMMAND: &str =
        "SELECT template FROM ban_messages WHERE guild_id = $1";

    pub async fn lookup(data: &crate::Data, gid: ser::GuildId) -> Result<Option<Self>> {
        let conn = data.dbconn.get().await?;
        let row = conn
            .query_opt(Self::LOOKUP_COMMAND, &[&u64::from(gid).to_be_bytes()])
            .await?;

        let row = match row {
            Some(r) => Some(Self {
                template: r.try_get(0).unwrap()
            }),
            None => None,
        };

        Ok(row)
    }

    const UPDATE_COMMAND: &str = "INSERT INTO ban_messages (guild_id, template) VALUES ($1, $2) ON CONFLICT (guild_id) DO UPDATE SET template = $2";
    pub async fn update(&self, data: &crate::Data, gid: ser::GuildId) -> Result<()> {
        let mut conn = data.dbconn.get().await?;
        let trans = conn.transaction().await?;
        trans.execute(Self::UPDATE_COMMAND, &[&u64::from(gid).to_be_bytes(), &self.template])
            .await?;

        trans.commit().await?;
        Ok(())
    }
}

#[derive(Debug)]
pub enum BanContext {
    DmingUser(ser::UserId)
}

impl Display for BanContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BanContext::*;
        match self {
            DmingUser(user) =>
                write!(f, "notifying user {user_mention} of being banned",
                       user_mention = user.mention())
        }
    }
}

lazy_static! {
    static ref TEMPLATE_VAR_REGEX: Regex = Regex::new(r"\$([a-zA-Z]+)").unwrap();
}

pub async fn dm_user(
    ctx: &impl ser::CacheHttp,
    user: ser::UserId,
    guild: ser::GuildId,
    data: &crate::Data,
    reason: &str,
) -> Result<()> {
    let settings = BanSettings::lookup(data, guild)
        .await?
    .unwrap_or_else(|| BanSettings {
        template: format!("You have been banned from {guild_name} for the following reason: $reason",
                          guild_name = match guild.name(ctx.cache().unwrap()) {
                              Some(g) => g,
                              None => guild.link(()).to_string()
                          })
        });

    
    let message_text = TEMPLATE_VAR_REGEX.replace_all(&settings.template, |m: &Captures<'_>| match m.extract().1 {
        ["reason"] => Cow::Borrowed(&*reason),
        [unknown] => Cow::Owned(format!("${unknown}"))
    });

    let channel = user.create_dm_channel(ctx).await?;
    channel.send_message(ctx.http(), |m| m.content(message_text)).await?;
    Ok(())
}

/// Bans a user.
#[poise::command(slash_command, guild_only = true, default_member_permissions = "BAN_MEMBERS")]
pub async fn ban(
    ctx: PoiseContext<'_>,
    #[description = "User to ban"]
    user: ser::UserId,
    #[description = "Ban reason"]
    reason: String,
    #[description = "Delete message in the last days"]
    #[min = 0]
    #[max = 7]
    dmd: Option<u8>
)  -> Result<()> {
    let dmd = dmd.unwrap_or(0);
    let guild = ctx.guild_id().unwrap();
    let dmresult = dm_user(&ctx, user, guild, &ctx.data(), &reason)
        .await
        .contextualize(BanContext::DmingUser(user));
    
    if let Err(err) = dmresult {
        ctx.send(|e| {
            e.content(format!("error: {err}")).ephemeral(true)
        }).await?;
    }

    guild.ban_with_reason(ctx, user, dmd, format!("{reason} (banned by {banner})",
                                                  banner = ctx.author().id.mention())).await?;
    ctx.send(|e| e.content("User successfully banned").ephemeral(true))
        .await?;
    Ok(())
}

pub mod admin {
    use crate::{ser, PoiseContext, errors::Result};
    use super::BanSettings;

    /// Configures the ban message.
    #[poise::command(slash_command, guild_only = true, default_member_permissions = "MANAGE_GUILD")]
    pub async fn ban(
        ctx: PoiseContext<'_>,
        #[description = "Link to a message containing a template to send"]
        template: ser::Message
    ) -> Result<()> {
        let settings = BanSettings {
            template: template.content
        };
        settings.update(ctx.data(), ctx.guild_id().unwrap()).await?;
        ctx.send(|e| e.content("Settings successfully updated!")).await?;
        Ok(())
    }
}

