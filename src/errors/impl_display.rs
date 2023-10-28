use std::fmt::{self, Display};
use crate::errors::Contextualizable;
use poise::serenity_prelude as ser;

use super::{Context, InternalError, OptError, Error, LoggedFrameworkError, LoggedContext, LoggedError, LoggedMappedWithContext, WithContext};

impl<T: Display> Display for OptError<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self.into() {
			Some(err) => write!(f, "{err}"),
			None => write!(f, "no error")
		}
	}
}

impl Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Context::*;
        match self {
			Log(logctx) => write!(f, "{logctx}"),
            Webhook(webhctx) => write!(f, "{webhctx}"),
            Replication(repctx) => write!(f, "{repctx}"),
            Purge(purgectx) => write!(f, "{purgectx}"),
			Remind(remindctx) => write!(f, "{remindctx}"),
		}
    }
}

impl Display for InternalError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use InternalError::*;
		match self {
			// the Display impl for ser::HttpError doesn't report _the actual error_ for Request. why?
			// i don't fucking know. just to spite me i guess
			// also why _the fuck_ is ser::Error::Http.0 in a Box???????
			SerenityError(ser::Error::Http(httperr)) => match &**httperr {
				ser::HttpError::Request(requesterr) =>
					write!(f, "Serenity HTTP error: {requesterr}"),
				err =>
					write!(f, "Serenity HTTP error: {err}"),
			}
			SerenityError(err) => write!(f, "Serenity error: {err}"),
			DatabaseError(err) => write!(f, "Database error: {err}"),
			Bb8Error(err) => write!(f, "bb8 error: {err}"),
			InvalidByteADiscordIDFormat => write!(f, "invalid bytea discord ID format"),
		}
	}
}

impl Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Error::*;
		match self {
			MissingPermission(permissions) =>
				write!(f, "Missing permissions: {permissions}"),
			RoleNotFound =>
				write!(f, "Role not found"),
			MemberNotFound =>
				write!(f, "Member not found"),
			ChannelNotFound =>
				write!(f, "Channel not found"),
			MessageAlreadyCrossposted =>
				write!(f, "Message already crossposted"),
			CannotCrosspostMessage =>
				write!(f, "Cannot crosspost message"),
			DateParseError(e) =>
				write!(f, "{e}"),
			GetLatencyError(e) =>
				write!(f, "{e}"),
			RemindError(e) =>
				write!(f, "{e}"),
			Log(e) =>
				write!(f, "{e}"),
			Internal(_) =>
				write!(f, "Internal error"),
		}
	}
}

impl<'a> Display for LoggedError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::*;
		match self.0 {
			Internal(ie) => write!(f, "internal error: {ie}"),
			err => write!(f, "{err}")
		}
    }
}

impl<'a, E, F> Display for LoggedMappedWithContext<'a, E, F>
where E: Contextualizable,
	  F: Display {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		for context in self.0.contexts.iter() {
			write!(f, "{context}: ")?;
		}

		write!(f, "{error}: ", error = self.1(&self.0.error))
	}
}

impl<'a> Display for LoggedContext<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "command {command}, ", command = self.0.invocation_string())?;
		if let Some(guild_id) = self.0.guild_id() {
			write!(f, "in server {guild_id}, ")?;
		}
		write!(f, "run by user {author_id} [{author_tag}]",
			   author_id = self.0.author().id,
			   author_tag = self.0.author().tag())
	}
}

pub(crate) fn withctx_error_logged(e: &WithContext<Error>) -> LoggedMappedWithContext<'_, Error, LoggedError> {
	LoggedMappedWithContext(e, LoggedError)
}

impl<'a, 'b> Display for LoggedFrameworkError<'a, 'b> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use poise::FrameworkError::*;
		match &self.0 {
			Setup { error, .. } =>
				write!(f, "poise setup error: {logged_error}",
					   logged_error = withctx_error_logged(error)),
			EventHandler { error, event, ..} =>
				write!(f, "error in {event_name}: {logged_error}",
					   event_name = event.name(),
					   logged_error = withctx_error_logged(error)),
			Command { error, ctx } =>
				write!(f, "{logged_ctx} returned error: {logged_error}",
					   logged_ctx = LoggedContext(*ctx),
					   logged_error = withctx_error_logged(error)),
			CommandPanic { payload: Some(payload), ctx } =>
				write!(f, "{logged_ctx} panicked: {payload}",
					   logged_ctx = LoggedContext(*ctx)),
			CommandPanic { payload: None, ctx } =>
				write!(f, "{logged_ctx} panicked without payload",
					   logged_ctx = LoggedContext(*ctx)),
			ArgumentParse { error, input: Some(input), ctx } =>
				write!(f, "error parsing the string `{input}' in {logged_ctx}: {error}",
					   logged_ctx = LoggedContext(*ctx)),
			ArgumentParse { error, input: None, ctx } =>
				write!(f, "error parsing input in {logged_ctx}: {error}",
					   logged_ctx = LoggedContext(*ctx)),
			CommandStructureMismatch { description, ctx } =>
				write!(f, "error in {logged_ctx}: {description}",
					   logged_ctx = LoggedContext((*ctx).into())),
			CooldownHit { remaining_cooldown, ctx } =>
				write!(f, "command cooldown of {seconds:.2} s for {logged_ctx}",
					   logged_ctx = LoggedContext(*ctx),
					   seconds = remaining_cooldown.as_secs_f32()),
			MissingBotPermissions { missing_permissions, ctx } =>
				write!(f, "{logged_ctx}: missing permissions for bot: {missing_permissions}",
					   logged_ctx = LoggedContext(*ctx)),
			MissingUserPermissions { missing_permissions: Some(missing_permissions), ctx } =>
				write!(f, "{logged_ctx}: missing permissions for user: {missing_permissions}",
					   logged_ctx = LoggedContext(*ctx)),
			MissingUserPermissions { missing_permissions: None, ctx } =>
				write!(f, "{logged_ctx}: missing permissions for user",
					   logged_ctx = LoggedContext(*ctx)),
			NotAnOwner { ctx } =>
				write!(f, "{logged_ctx}: user is not an owner",
					   logged_ctx = LoggedContext(*ctx)),
			GuildOnly { ctx } =>
				write!(f, "{logged_ctx}: can only be used in guilds",
					   logged_ctx = LoggedContext(*ctx)),
			DmOnly { ctx } =>
				write!(f, "{logged_ctx}: command can only be used in DMs",
					   logged_ctx = LoggedContext(*ctx)),
			NsfwOnly { ctx } =>
				write!(f, "{logged_ctx}: command can only be used in NSFW channels",
					   logged_ctx = LoggedContext(*ctx)),
			CommandCheckFailed { error: Some(error), ctx } =>
				write!(f, "provided pre-check failed for {logged_ctx}: {logged_error}",
					   logged_ctx = LoggedContext(*ctx),
					   logged_error = withctx_error_logged(error)),
			DynamicPrefix { error, msg, .. } =>
				write!(f, "dynamic prefix callback returned an error on message {msg_content:?}: {logged_error}",
					   msg_content = msg.content,
					   logged_error = withctx_error_logged(error)),
			UnknownCommand { msg_content, .. } =>
				write!(f, "unknown command: {msg_content:?}"),
			UnknownInteraction { interaction, .. } =>
				write!(f, "unkown interaction: {interaction_id}",
					   interaction_id = interaction.id()),
			err => write!(f, "unkown (by the built-in suzu error formatter) error: {err}"),
		}
	}
}
