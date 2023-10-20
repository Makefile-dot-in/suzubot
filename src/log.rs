use std::fmt::Display;
use std::error::Error as StdError;
use std::hash::Hash;

use std::fmt;


use crate::{pg, ts_to_id};
use crate::pgtyp::{FromSql, ToSql};
use crate::PoiseContext;
use crate::errors::{Result, Error, InternalError, Contextualizable, AsyncReportErr};
use crate::msgreplication;
use crate::linkable::Linkable;
use crate::truncate;
use crate::fmttime_discord;

use chrono::Duration;
use poise::serenity_prelude::{CreateEmbed, Cache, CacheHttp};
use poise::{serenity_prelude as ser, ChoiceParameter, Event, FrameworkContext};
use ser::Mentionable;
use std::sync::Mutex;
use std::collections::{HashSet, HashMap};
use std::collections::hash_map::Entry;




pub const GET_LOGCH: &str = "SELECT channel_id FROM log_channels WHERE server_id = $1 AND log_typ = $2;";
pub const SET_LOGCH: &str = "INSERT INTO log_channels (server_id, log_typ, channel_id)
   VALUES ($1, $2, $3)
   ON CONFLICT ON CONSTRAINT log_channels_pkey
     DO UPDATE SET channel_id = EXCLUDED.channel_id;";
pub const DEL_LOGCH: &str = "DELETE FROM log_channels WHERE server_id = $1 AND log_typ = $2;";

#[derive(Clone, Copy, PartialEq, Eq, Debug, ToSql, FromSql, ChoiceParameter)]
#[postgres(name = "logtype")]
pub enum LogType {
	#[name = "purge"]
    Purge,
	#[name = "bot_config"]
	BotConfig,
	
	#[name = "message_edit"]
	MessageEdit,
	#[name = "message_delete"]
	MessageDelete,

	#[name = "user_ban"]
	UserBan,
	#[name = "user_kick"]
	UserKick,

	#[name = "user_join"]
	UserJoin,
	#[name = "user_leave"]
	UserLeave,

	#[name = "voice_update"]
	VoiceUpdate,
}

impl LogType {
	fn color(self) -> ser::Color {
		use LogType::*;
		use ser::Color;
		match self {
			Purge => Color::GOLD,
			BotConfig => Color::KERBAL,
			MessageEdit => Color::BLITZ_BLUE,
			MessageDelete => Color::RED,
			UserBan => Color::DARK_RED,
			UserKick => Color::FOOYOO,
			UserJoin => Color::DARK_BLUE,
			UserLeave => Color::FABLED_PINK,
			VoiceUpdate => Color::TEAL,
		}
	}

	fn title(self) -> &'static str {
		use LogType::*;
		match self {
			Purge => "🔥 Purged Messages",
			BotConfig => "🔧 Bot Config Changed",
			MessageEdit => "📝 Edited Message",
			MessageDelete => "🗑️ Deleted Message",
			UserBan => "🚫 User Banned",
			UserKick => "🏌️ User Kicked",
			UserJoin => "👤 User Joined",
			UserLeave => "🚶 User Left",
			VoiceUpdate => "� Voice State Update",
		}
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

#[derive(Debug)]
pub struct LogData {
	monopolized_messages: Mutex<HashSet<ser::MessageId>>,
	newest_entry_id: Mutex<HashMap<u64, ser::AuditLogEntryId>>
}

impl LogData {
	pub fn new() -> Self {
		Self {
			monopolized_messages: Mutex::new(HashSet::new()),
			newest_entry_id: Mutex::new(HashMap::new()),
		}
	}
}

#[derive(Debug, Clone)]
pub struct MonopolizeGuard<'a, T: Clone + Hash + Eq> {
	items: Vec<T>,
	itemset: &'a Mutex<HashSet<T>>,
}

impl LogData {
	pub fn monopolize_messages<'a, M>(&'a self, msgs: M) -> MonopolizeGuard<'a, ser::MessageId>
	where M: IntoIterator<Item = ser::MessageId> {
		let mut items = Vec::new();
		let mut monmsgs = self.monopolized_messages.lock().unwrap();
		for msg in msgs {
			if monmsgs.insert(msg) {
				items.push(msg);
			}
		}
		MonopolizeGuard {
			items,
			itemset: &self.monopolized_messages
		}
	}

	/// checks whether check_relevancy(u, entry) hasn't been already called on this audit log or a newer one, returning true if it hasn't
	fn check_relevancy(&self, u: u64, entry: ser::AuditLogEntryId) -> bool {
		let mut map = self.newest_entry_id.lock().unwrap();
		match map.entry(u) {
			Entry::Vacant(e) => {
				e.insert(u.into());
				true
			},
			Entry::Occupied(mut e) if entry > *e.get() => {
				e.insert(u.into());
				true
			},
			_ => false,
		}
	}
}


impl<'a, T: Clone + Eq + Hash> Drop for MonopolizeGuard<'a, T> {
    fn drop(&mut self) {
		let mut itemg = self.itemset.lock().unwrap();
        for item in self.items.iter() {
			itemg.remove(item);
		}
    }
}

impl<'a, T: Clone + Eq + Hash> MonopolizeGuard<'a, T> {
	pub fn items(&self) -> impl Iterator<Item = &T> {
		self.items.iter()
	}
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


	logch.send_message(http, |m| {
		m.embed(|e| {
			builder(e.title(log_type.title()).color(log_type.color()))
		}).allowed_mentions(|am| am.empty_parse())
	}).await
		.map_err(Error::from)
		.contextualize(LogErrorContext::InChannel(logch))
}

pub fn user_to_embed_author<'a>(
	user: &ser::User,
	a: &'a mut ser::CreateEmbedAuthor
) -> &'a mut ser::CreateEmbedAuthor {
	a.name(&user.name);

	if let Some(avatar_url) = &user.avatar {
		a.icon_url(avatar_url);
	}
	
	a
}

/// retrieves the latest audit log entry for a particular action to augment log messages
/// returns a tuple of (entry, action_performer) where action_performer is an [[Option<ser::User>]] retrieved from the cache
async fn get_audit_data(
	client: impl CacheHttp,
	data: &crate::Data,
	guild: impl Into<ser::GuildId>,
	action: ser::audit_log::Action,
	target: impl Into<u64>
) -> (Option<ser::AuditLogEntry>, Option<ser::User>) {
	let guild: ser::GuildId = guild.into();
	let target: u64 = target.into();
	let entry = guild.audit_logs(
		client.http(),
		Some(action.num()),
		None,
		Some(ts_to_id(ser::Timestamp::now())),
		Some(1)
	).await
		.inspect_err(|e| log::error!("retrieving audit log in server {guild}: {e}"))
		.ok()
		.and_then(|l| l.entries.into_iter().next())
		.filter(|l| l.target_id.is_some_and(|t| t == target))
		.filter(|l| data.logdata.check_relevancy(target, l.id))
		.filter(|l| (*l.id.created_at() - *ser::Timestamp::now()) < Duration::minutes(5)); // we don't connect log events with audit log events more than 5 minutes ago
	let action_performer = entry.as_ref()
		.zip(client.cache())
		.and_then(|(l, c)| c.user(l.user_id));
	(entry, action_performer)
}
	

pub(crate) async fn log_purge<M>(
	ctx: PoiseContext<'_>,
	guild: ser::GuildId,
	limit: u16,
	total: u16,
	inlast: u16,
	author: Option<ser::UserId>,
	before: Option<ser::MessageId>,
	after: Option<ser::MessageId>,
	pattern: Option<String>,
	reason: Option<String>,
	messages: M
) -> Result<()>
where M: IntoIterator<Item = ser::Message> {
	let message = post_log(
		ctx,
		ctx.data(),
		guild,
		LogType::Purge,
		|e| {
			e.field("Purger", ctx.author().mention(), true)
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
			&ctx.data().bot_name,
			&ctx.data().webhexec,
			message.channel_id,
			Some(thread),
			messages,
			|x| x
		).await?
	};
	result.contextualize(LogErrorContext::InChannel(message.channel_id))
}

pub async fn log_bot_config(
	ctx: PoiseContext<'_>,
	setting: impl ToString + Send,
	builder: impl FnOnce(&mut CreateEmbed) -> &mut CreateEmbed + Send
) -> Result<()> {
	post_log(
		ctx,
		ctx.data(),
		ctx.guild_id().unwrap(),
		LogType::BotConfig,
		|e| {
			builder({
				e.field("User", ctx.author().mention(), true)
					.field("Setting", setting, true)
			})
		}
	).await?;
	Ok(())
}

pub async fn log_message_edit(
	http: impl AsRef<ser::Http>,
	data: &crate::Data,
	old: &Option<ser::Message>,
	evt: &ser::MessageUpdateEvent
) -> Result<()> {
	let monguard = data.logdata.monopolize_messages([evt.id]);
	if !monguard.items.contains(&evt.id) { return Ok(()); };

	post_log(
		http,
		data,
		evt.guild_id.unwrap(),
		LogType::MessageEdit,
		|e| {			
			if let Some(author) = &evt.author {
				e.author(|a| user_to_embed_author(&author, a));
				e.field("Author", author.mention(), true);
			}

			e.field("Channel", evt.channel_id.mention(), true);

			if let Some(old_message) = old {
				e.field("Old", truncate(old_message.content.clone(), 1024), false);
			}

			if let Some(new_contents) = &evt.content {
				e.field("New", truncate(new_contents.clone(), 1024), false);
			}
			e.field("Link", evt.id.link((evt.guild_id, evt.channel_id)), false);
			e
		}
	).await?;
	Ok(())
}

pub async fn log_message_delete(
	client: impl ser::CacheHttp,
	data: &crate::Data,
	channel_id: ser::ChannelId,
	deleted_msg_id: ser::MessageId,
	guild_id: Option<ser::GuildId>
) -> Result<()> {
	let monguard = data.logdata.monopolize_messages([deleted_msg_id]);
	if !monguard.items.contains(&deleted_msg_id) { return Ok(()); }

	let cached = client.cache().and_then(|c| c.message(channel_id, deleted_msg_id));
	post_log(
		client.http(),
		data,
		guild_id.unwrap(),
		LogType::MessageDelete,
		|e| {
			if let Some(message) = &cached {
				e.author(|a| user_to_embed_author(&message.author, a));
				e.field("Author", message.author.mention(), true);
			}
			
			e.field("Channel", channel_id.mention(), true);

			if let Some(message) = cached {
				e.field("Old", message.content, false);
			}
			
			e.field("Link", deleted_msg_id.link((guild_id, channel_id)), false);
			e
		}
	).await?;
	Ok(())
}


pub async fn log_user_ban(
	client: impl CacheHttp,
	data: &crate::Data,
	guild_id: ser::GuildId,
	user: &ser::User
) -> Result<()> {
	use ser::audit_log::{Action, MemberAction};
	let (log_data_opt, banned_by_opt) = get_audit_data(
		client.http(),
		data,
		guild_id,
		Action::Member(MemberAction::BanAdd),
		user.id
  	).await;
	
	post_log(
		client.http(),
		data,
		guild_id,
		LogType::UserBan,
		|e| {
			e.author(|a| user_to_embed_author(&user, a))
				.field("User", user.mention(), true);

			if let Some(log_data) = log_data_opt {
				if let Some(banned_by) = banned_by_opt {
					e.author(|a| user_to_embed_author(&banned_by, a));
				}

				e.field("Banned by", log_data.user_id.mention(), true);
				if let Some(reason) = log_data.reason {
					e.field("Reason", reason, false);
				}
			}
			e
		}
	).await?;
	Ok(())
}

	

pub async fn log_user_join(
	http: impl AsRef<ser::Http>,
	data: &crate::Data,
	new_member: &ser::Member
) -> Result<()> {
	post_log(
		http,
		data,
		new_member.guild_id,
		LogType::UserJoin,
		|e| {
			e.author(|a| user_to_embed_author(&new_member.user, a))
				.field("User", new_member.user.mention(), true)
				.field("Account created", fmttime_discord(new_member.user.created_at()), true)
		}
	).await?;
	Ok(())
}
pub async fn log_user_remove(
	client: impl CacheHttp,
	data: &crate::Data,
	guild_id: ser::GuildId,
	user: &ser::User,
	member: &Option<ser::Member>
) -> Result<()> {
	use ser::audit_log::{Action, MemberAction};
	let (log_data_opt, kicked_by_opt) = get_audit_data(
		&client,
		data,
		guild_id,
		Action::Member(MemberAction::Kick),
		user.id
	).await;
	let log_type = match log_data_opt {
		Some(_) => LogType::UserKick,
		None => LogType::UserLeave,
	};
	post_log(
		client.http(),
		data,
		guild_id,
		log_type,
		|e| {
			e.author(|a| user_to_embed_author(&user, a))
				.field("User", user.mention(), true)
				.field("Account created", fmttime_discord(user.created_at()), true);
			if let Some(joined_at) = member.as_ref().and_then(|m| m.joined_at) {
				e.field("Joined server", fmttime_discord(joined_at), true);
			}

			if let Some(log_data) = log_data_opt {
				if let Some(kicked_by) = kicked_by_opt {
					e.author(|a| user_to_embed_author(&kicked_by, a));
				}
				e.field("Kicked by", log_data.user_id.mention(), true);

				if let Some(reason) = log_data.reason {
					e.field("Reason", reason, false);
				}
			} else {
				e.author(|a| user_to_embed_author(&user, a));
			}
			
			e
		}
	).await?;
	Ok(())
}

pub async fn log_voice_update(
	http: impl AsRef<ser::Http>,
	data: &crate::Data,
	new_state: &ser::VoiceState,
	old_state_opt: &Option<ser::VoiceState>
) -> Result<()> {
	let Some(old_state) = old_state_opt else { return Ok(()); };
	let states = [old_state, new_state];
	let loggable_transitions = match states {
		[ser::VoiceState { channel_id: old_chid, .. },
		 ser::VoiceState { channel_id: new_chid, .. }]
			if old_chid == new_chid => vec![],
		[ser::VoiceState { guild_id: Some(old_guid),
						   channel_id: old_chid, .. },
		 ser::VoiceState { guild_id: Some(new_guid),
						   channel_id: new_chid, .. }]
			if old_guid == new_guid => vec![(*old_guid, *old_chid, *new_chid)],
		[ser::VoiceState { guild_id: Some(old_guid),
						   channel_id: Some(old_chid), .. },
		 ser::VoiceState { guild_id: Some(new_guid),
						   channel_id: Some(new_chid), .. }]
			if old_guid != new_guid => vec![(*old_guid, Some(*old_chid), None),
											(*new_guid, None, Some(*new_chid))],
		_ => vec![]
	};
	for (guild_id, old_vc, new_vc) in loggable_transitions {
		post_log(
			http.as_ref(),
			data,
			guild_id,
			LogType::VoiceUpdate,
			|e| {
				let [old_vc_str, new_vc_str] = [old_vc, new_vc]
					.map(|vc_opt| {
						match vc_opt {
							Some(vc) => vc.mention().to_string(),
							None => "(none)".to_string()
						}
					});
				
				e.field("User", new_state.user_id.mention(), true)
					.field("Previous VC", old_vc_str, true)
					.field("New VC", new_vc_str, true);

				if let Some(author) = new_state.member.as_ref().map(|m| &m.user) {
					e.author(|a| user_to_embed_author(author, a));
				}

				e.field("User", new_state.user_id.mention(), true);
				
				e
			}
		).await?;
	}
	Ok(())
}

pub async fn log_event<'a>(
	ctx: &'a ser::Context,
	evt: &'a Event<'a>,
	_: FrameworkContext<'a, crate::Data, Error>,
	data: &crate::Data
) -> Result<()>	{
	use Event::*;
	match evt {
		MessageUpdate { old_if_available, new: _, event } =>
			log_message_edit(ctx, data, old_if_available, event).await?,
		MessageDelete { channel_id, deleted_message_id, guild_id } =>
			log_message_delete(ctx, data, *channel_id, *deleted_message_id, *guild_id).await?,
		GuildBanAddition { guild_id, banned_user } =>
			log_user_ban(ctx, data, *guild_id, banned_user).await?,
		GuildMemberAddition { new_member } =>
			log_user_join(ctx, data, new_member).await?,
		GuildMemberRemoval { guild_id, user, member_data_if_available } =>
			log_user_remove(ctx, data, *guild_id, user, member_data_if_available).await?,
		VoiceStateUpdate { old, new } =>
			log_voice_update(ctx, data, new, old).await?,
		_ => {},
	}
	Ok(())
}

/// Sets the channel for a log type.
#[poise::command(slash_command, guild_only = true, default_member_permissions = "MANAGE_GUILD")]
pub async fn log(
	ctx: PoiseContext<'_>,
	#[description = "Type of events to log"]
	log_type: LogType,
	#[description = "Channel to log events. Don't specify to disable logging for the specified log_type"]
	channel: Option<ser::ChannelId>
) -> Result<()> {
	let guild_id = ctx.guild_id().unwrap();
	ctx.defer()
		.await
		.map_err(Error::from)?;
	match channel {
		Some(chid) => {
			set_logch(ctx.data(), guild_id, log_type, chid).await
				.contextualize(LogErrorContext::SettingLogChannel(guild_id, log_type, chid))
				.report_err(|err| ctx.say(err))
				.await?;
			ctx.say(format!("Successfully set the channel for `{log_type}` to {channel_mention}",
							channel_mention = chid.mention()))
				.await?;
		}
		None => {
			del_logch(ctx.data(), guild_id, log_type).await
				.contextualize(LogErrorContext::DisablingLogging(guild_id, log_type))
				.report_err(|err| ctx.say(err))
				.await?;
			ctx.say(format!("Successfully disabled logging for `{log_type}`."))
				.await?;
		}
	}


	log_bot_config(ctx, "log", |e| {
		e.field("Log type", log_type, true);
		match channel {
			Some(chid) => e.field("State", "Enabled", true)
				.field("Channel", chid.mention(), true),
			None => e.field("State", "Disabled", true)
		}
	}).await.ok();
	Ok(())
}
