use std::collections::{HashMap, hash_map};
use std::convert::identity;
use std::result::Result as StdResult;
use std::fmt::Display;
use std::error::Error as StdError;
use std::hash::Hash;
use std::marker::PhantomData;
use std::fmt;
use std::str::FromStr;

use crate::pg;
use crate::pgtyp::{FromSql, ToSql};
use crate::PoiseContext;
use crate::errors::{Result, Error, InternalError, Contextualizable, CmdResult, WithContext, OptError, AsyncReportErr};
use crate::msgreplication;
use crate::linkable::Linkable;
use async_trait::async_trait;
use futures::{FutureExt, TryFutureExt};
use poise::serenity_prelude::guild;
use poise::{serenity_prelude as ser, SlashArgument, SlashArgError, extract_slash_argument, ApplicationCommandOrAutocompleteInteraction, ChoiceParameter};
use ser::Mentionable;
use tokio::sync::RwLock;



pub const GET_LOGCH: &str = "SELECT channel_id FROM log_channels WHERE server_id = $1 AND log_type = $2;";
pub const SET_LOGCH: &str = "INSERT INTO log_channels (server_id, log_type, channel_id)
   VALUES ($1, $2, $3)
   ON CONFLICT ON CONSTRAINT log_channels_pkey
     DO UPDATE SET channel_id = EXCLUDED.channel_id;";
pub const DEL_LOGCH: &str = "DELETE FROM log_channels WHERE server_id = $1 AND log_type = $2;";

#[derive(Clone, Copy, PartialEq, Eq, Debug, ToSql, FromSql, ChoiceParameter)]
pub enum LogType {
	#[name = "purge"]
    Purge,
	#[name = "bot_config"]
	BotConfig,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum LogEventType {
	Delete = 0,
	Create = 1,
	Edit = 2,
}

impl From<LogEventType> for u8 {
	fn from(value: LogEventType) -> u8 {
		value as u8
	}
}

#[derive(Debug)]
pub enum LogErrorContext {
	Log(ser::GuildId, LogType),
	InChannel(ser::ChannelId),
	GettingLogChannel,
	CreatingPurgedMessageThread,
	SettingLogChannel(ser::GuildId, LogType, ser::ChannelId),
	DisablingLogging(ser::GuildId, LogType)
}

impl Display for LogErrorContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use LogErrorContext::*;
		match self {
			Log(guild, log_type) =>
				write!(f, "logging log type `{log_type}` in {guild_link}",
					   guild_link = guild.link(())),
			InChannel(channel) =>
				write!(f, "logging to `{channel_mention}`",
					   channel_mention = channel.mention()),
			GettingLogChannel =>
				write!(f, "retrieving log channel"),
			CreatingPurgedMessageThread =>
				write!(f, "creating purged message thread"),
			SettingLogChannel(guild, log_type, channel) =>
				write!(f, "setting log channel in server {guild_link} for log type `{log_type}` to {channel_mention} ",
					   guild_link = guild.link(()),
					   channel_mention = channel.mention()),
			DisablingLogging(guild, log_type) =>
				write!(f, "disabling logging for {log_type} in server {guild_link}",
					   guild_link = guild.link(()))
		}
    }
}


#[derive(Debug)]
pub enum LogError {
	NoLogChannel,
	InvalidLogType(Box<str>),
}

impl Display for LogError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
			Self::NoLogChannel =>
				write!(f, "no log channel set up for log type"),
			Self::InvalidLogType(t) =>
				write!(f, "invalid log type: {t}")
		}
    }
}

impl StdError for LogError {}



/// thread-safe map from any type to a mask of any type that supports Into<u8>
pub struct MaskMap<T: Eq + Hash, M: Into<u8>>(RwLock<HashMap<T, u32>>, PhantomData<M>);

impl<T: Eq + Hash, M: Into<u8>> MaskMap<T, M> {
	pub fn new() -> Self {
		Self(RwLock::new(HashMap::new()), PhantomData)
	}

	pub async fn set(&mut self, key: T, log_type: M) {
		*self.0.write().await.entry(key).or_insert(0) |= 1 << log_type.into();
	}

	pub async fn unset(&mut self, key: T, log_type: M) {
		use hash_map::Entry::*;
		match self.0.write().await.entry(key) {
			Occupied(mut occ) => {
				let mask = occ.get_mut();
				*mask &= !(1 << log_type.into());
				if *mask == 0 {
					occ.remove_entry();
				}
			},
			Vacant(_) => {},
		}
	}

	pub async fn check(&mut self, key: T, log_type: M) -> bool {
		let val = self.0.read().await.get(&key).map(|x| *x).unwrap_or(0);
		(val >> log_type.into()) & 1 == 1
	}
}

/// prevents logging for a set of messages
/// warning! not thread safe
pub struct LogPreventer<T: Eq + Hash> {
}

/// A collection of [`MaskMap`] used to prevent logging of things that are already being logged.
pub struct LogMasks {
	pub messages: MaskMap<ser::MessageId, LogEventType>
}

async fn get_logch(
    data: &crate::Data,
    guild: ser::GuildId,
    log_type: LogType,
) -> Result<Option<ser::ChannelId>> {
    let conn = data.dbconn.get().await?;
    let Some(row): Option<pg::Row> = conn
        .query_opt(GET_LOGCH, &[&guild.0.to_be_bytes(), &log_type])
        .await?
    else {
        return Ok(None);
    };

    let channelid: [u8; 8] = row
        .try_get::<_, Vec<u8>>(0)?
        .try_into()
        .map_err(|_| InternalError::InvalidByteADiscordIDFormat)?;

    Ok(Some(ser::ChannelId(u64::from_be_bytes(channelid))))
}

async fn set_logch(
	data: &crate::Data,
	guild: ser::GuildId,
	log_type: LogType,
	channel: ser::ChannelId
) -> Result<()> {
	let mut conn = data.dbconn.get().await?;
	let trans = conn.transaction().await?;
	trans.execute(SET_LOGCH, &[&guild.0.to_be_bytes(), &log_type, &channel.0.to_be_bytes()]).await?;
	trans.commit().await?;
	Ok(())
}

async fn del_logch(
	data: &crate::Data,
	guild: ser::GuildId,
	log_type: LogType
) -> Result<()> {
	let mut conn = data.dbconn.get().await?;
	let trans = conn.transaction().await?;
	trans.execute(DEL_LOGCH, &[&guild.0.to_be_bytes(), &log_type]).await?;
	trans.commit().await?;
	Ok(())
}
	

/// generalized log poster
async fn post_log<'b, F>(
	http: impl AsRef<ser::Http>,
	data: &crate::Data,
	guild: ser::GuildId,
	log_type: LogType,
	builder: F,
) -> Result<ser::Message>
where F: for<'a> FnOnce(&'a mut ser::CreateEmbed) -> &'a mut ser::CreateEmbed + Send
{
	let logch = get_logch(data, guild, log_type)
		.await
		.contextualize(LogErrorContext::GettingLogChannel)?
		.ok_or(LogError::NoLogChannel)?;


	logch.send_message(http, |m| m.embed(builder).allowed_mentions(|am| am.empty_parse()))
		.await
		.map_err(Error::from)
		.contextualize(LogErrorContext::InChannel(logch))
}

pub(crate) async fn log_purge(
	ctx: &PoiseContext<'_>,
	guild: ser::GuildId,
	limit: u16,
	total: u16,
	inlast: u16,
	author: Option<ser::UserId>,
	before: Option<ser::MessageId>,
	after: Option<ser::MessageId>,
	pattern: Option<String>,
	reason: Option<String>,
	messages: impl IntoIterator<Item = ser::Message> + Send
) -> Result<()> {
	let message = post_log(
		ctx,
		ctx.data(),
		guild,
		LogType::Purge,
		|e| {
			e
				.title("🔥 Purged Messages")
				.color(ser::Color::GOLD)
				.field("Purger", ctx.author().mention(), true)
				.field("Total", total, true)
				.field("Limit", limit, true)
				.field("In last", inlast, true)
				.field("Channel", ctx.channel_id(), true);

			if let Some(author) = author {
				e.field("By author", author.mention(), true);
			}

			if let Some(before) = before {
				e.field(
					"Before",
					format!("{before_link} (<t:{before_ts}>)",
							before_link = before.link((Some(guild), ctx.channel_id())),
							before_ts = before.created_at().timestamp()),
					true
				);
			}

			if let Some(after) = after {
				e.field(
					"After",
					format!("{after_link} (<t:{after_ts}>)",
							after_link = after.link((Some(guild), ctx.channel_id())),
							after_ts = after.created_at().timestamp()),
					true
				);
			}

			if let Some(pattern) = pattern {
				e.field("Pattern", format!("``{pattern}``"), true);
			}

			if let Some(reason) = reason {
				e.field("Reason", format!("{reason}"), false);
			}
			
			e
		}).await?;

	let result: Result<()> = try {
		let thread = message
			.channel_id
			.create_public_thread(ctx, message.id, |t| t.name("Purged messages"))
			.await
			.map_err(Error::from)
			.contextualize(LogErrorContext::CreatingPurgedMessageThread)?;

		msgreplication::replicate_messages(
			ctx,
			&ctx.data().webhexec,
			message.channel_id,
			Some(thread),
			messages,
			|x| x
		).await?
	};
	result.contextualize(LogErrorContext::InChannel(message.channel_id))
}

/// Sets the channel for a log type.
#[poise::command(slash_command, guild_only = true, default_member_permissions = "MANAGE_GUILD")]
pub async fn log(
	ctx: PoiseContext<'_>,
	#[description = "Type of events to log"]
	log_type: LogType,
	#[description = "Channel to log events. Don't specify to disable logging for the specified log_type"]
	channel: Option<ser::ChannelId>
) -> CmdResult<()> {
	let guild_id = ctx.guild_id().unwrap();
	ctx.defer()
		.await
		.map_err(Error::from)?;
	match channel {
		Some(chid) => {
			set_logch(ctx.data(), guild_id, log_type, chid).await
				.contextualize(LogErrorContext::SettingLogChannel(guild_id, log_type, chid))
				.report_err(|err| ctx.say(err))
				.await
				.map_contextualized(OptError::<InternalError>::from)?;
			ctx.say(format!("Successfully set the channel for `{log_type}` to {channel_mention}",
							channel_mention = chid.mention()))
				.await
				.map_err(Error::from)?;
		}
		None => {
			del_logch(ctx.data(), guild_id, log_type).await
				.contextualize(LogErrorContext::DisablingLogging(guild_id, log_type))
				.report_err(|err| ctx.say(err))
				.await
				.map_contextualized(OptError::<InternalError>::from)?;
			ctx.say(format!("Successfully disabled logging for `{log_type}`."))
				.await
				.map_err(Error::from)?;
		}
	}

	Ok(())
}
