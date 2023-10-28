use itertools::Itertools;
use poise::{FrameworkError, ApplicationCommandOrAutocompleteInteraction};
use poise::serenity_prelude as ser;

use crate::errors::LogError;

use super::Error;
use super::LoggedFrameworkError;

fn suzu_error_level(err: &Error) -> log::Level {
	match err {
		Error::Internal(_) => log::Level::Warn,
		_ => log::Level::Info,
	}
}

fn framework_error_level(err: &FrameworkError<'_, crate::Data, crate::SuzuError>) -> log::Level {
	use FrameworkError::*;
	match err {
		Setup { .. } => log::Level::Error,
		EventHandler { error, .. } => suzu_error_level(&error.error),
		Command { error, .. } => suzu_error_level(&error.error),
		SubcommandRequired { ctx } if ctx.command().subcommands.is_empty() => log::Level::Error,
		SubcommandRequired { .. } => log::Level::Info,
		CommandPanic { .. } => log::Level::Error,
		ArgumentParse { .. } => log::Level::Info,
		CommandStructureMismatch { .. } => log::Level::Warn,
		CooldownHit { .. } => log::Level::Info,
		MissingBotPermissions { .. } => log::Level::Info,
		MissingUserPermissions { .. } => log::Level::Info,
		NotAnOwner { .. } => log::Level::Info,
		GuildOnly { .. } => log::Level::Info,
		DmOnly { .. } => log::Level::Info,
		NsfwOnly { .. } => log::Level::Info,
		CommandCheckFailed { error: Some(error), .. } => suzu_error_level(&error.error),
		CommandCheckFailed { error: None, .. } => log::Level::Info,
		DynamicPrefix { error, .. } => suzu_error_level(&error.error),
		UnknownCommand { .. } => log::Level::Warn,
		UnknownInteraction { .. } => log::Level::Warn,
		_ => log::Level::Warn,
	}
}

async fn discord_report_message(err: &FrameworkError<'_, crate::Data, crate::SuzuError>) {
	use FrameworkError::*;
	match err {
		Command { ctx, error } => {
			ctx.say(format!("Error: {error}")).await.logerr();
		},
		SubcommandRequired { ctx } => {
			let subcommand_string = match ctx.command().subcommands.as_slice() {
				[cmd] => format!("`{cmd_name}`", cmd_name = cmd.name),
				[cmd1, cmd2] => format!("Either `{cmd1_name}` or `{cmd2_name}`",
										cmd1_name = cmd1.name,
										cmd2_name = cmd2.name),
				[cmds @ .., last_cmd] => format!("Either `{cmd_names}`, or `{last_cmd_name}`",
												 cmd_names = cmds.iter().map(|c| &*c.name).join(", "),
												 last_cmd_name = last_cmd.name),
				[] => "Huh. There are no subcommands. \
					   I guess you just can't use this command then."
					.to_owned()
			};
			ctx.say(format!("{command} requires a subcommand: {subcommand_string}",
							command = ctx.invoked_command_name())).await.logerr();
		},
		CommandPanic { ctx, .. } => {
			ctx.say("Command crashed.").await.logerr();
		},
		ArgumentParse { error, input: Some(input), ctx, .. } => {
			ctx.send(|m| {
				m.content(format!("Could not parse the input `{input}`: {error}"))
				 .allowed_mentions(|am| am.empty_parse())
			}).await.logerr();
		},
		ArgumentParse { error, input: None, ctx, .. } => {
			ctx.say(format!("Parsing error: {error}"))
			   .await
			   .logerr();
		},
		CommandStructureMismatch { ctx, .. } => {
			ctx.say("Command failed due to a bot configuration error.").await.logerr();
		},
		CooldownHit { remaining_cooldown, ctx, .. } => {
			ctx.say(format!("Cooldown not yet expired ({seconds} seconds remaining)",
							seconds = remaining_cooldown.as_secs())).await.logerr();
		},
		MissingBotPermissions { missing_permissions, ctx, .. } => {
			ctx.say(format!("The bot is missing permissions: {missing_permissions}.")).await.logerr();
		},
		MissingUserPermissions { missing_permissions: Some(perms), ctx, .. } => {
			ctx.say(format!("You need the following permissions to run this command: {perms}")).await.logerr();
		},
		MissingUserPermissions { missing_permissions: None, ctx, .. } => {
			ctx.say(format!("The bot couldn't retrieve your permissions, so to be safe, \
							 you can't run the command, as it requires the {cmd_perms} permission(s).",
							cmd_perms = ctx.command().required_permissions)).await.logerr();
		},
		GuildOnly { ctx, .. } => {
			ctx.say("This command can only be used in servers.").await.logerr();
		},
		DmOnly { ctx, .. } => {
			ctx.say("This command may only be used in DMs.").await.logerr();
		},
		NsfwOnly { ctx, .. } => {
			ctx.say("This command may only be used in NSFW channels.").await.logerr();
		},

		CommandCheckFailed { error: Some(error), ctx } => {
			ctx.say(format!("{error}")).await.logerr();
		},
		CommandCheckFailed { error: None, ctx } => {
			ctx.say("Pre-command check failed.").await.logerr();
		},
		UnknownInteraction { ctx, interaction: ApplicationCommandOrAutocompleteInteraction::ApplicationCommand(interaction), .. } => {
			interaction.create_interaction_response(ctx, |r| {
				r.kind(ser::InteractionResponseType::ChannelMessageWithSource)
					.interaction_response_data(|d| d.content("Unknown interaction"))
			}).await.logerr();
		},
		UnknownInteraction { interaction: ApplicationCommandOrAutocompleteInteraction::Autocomplete(_), .. } => {},
		_ => {}
	};
}

pub async fn on_error(err: FrameworkError<'_, crate::Data, crate::SuzuError>) {
	let log_level = framework_error_level(&err);
	log::log!(log_level, "{logged_err}",
			  logged_err = LoggedFrameworkError(&err));

	discord_report_message(&err).await;
}
