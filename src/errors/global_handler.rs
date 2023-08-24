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

fn framework_error_level(err: &FrameworkError<'_, crate::SuzuData, crate::SuzuError>) -> log::Level {
	use FrameworkError::*;
	match err {
		Setup { .. } => log::Level::Error,
		EventHandler { error, .. } => suzu_error_level(&error.error),
		Command { error, .. } => suzu_error_level(&error.error),
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

async fn discord_report_message(err: &FrameworkError<'_, crate::SuzuData, crate::SuzuError>) {
	use FrameworkError::*;
	match err {
		CommandPanic { ctx, .. } => {
			ctx.say("Command crashed.").await.logerr();
		},
		CommandStructureMismatch { ctx, .. } => {
			ctx.say("Command failed due to a bot configuration error.").await.logerr();
		},
		CooldownHit { remaining_cooldown, ctx, .. } => {
			ctx.say(format!("Cooldown not yet expired ({seconds} seconds remaining)",
							seconds = remaining_cooldown.as_secs())).await.logerr();
		},
		DmOnly { ctx, .. } => {
			ctx.say("This command may only be used in DMs.").await.logerr();
		},
		NsfwOnly { ctx, .. } => {
			ctx.say("This command may only be used in NSFW channels.").await.logerr();
		},
		MissingBotPermissions { missing_permissions, ctx } => {
			ctx.say(format!("The bot is missing permissions {missing_permissions}")).await.logerr();
		}
		CommandCheckFailed { error: Some(error), ctx } => {
			ctx.say(format!("{error}")).await.logerr();
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

pub async fn on_error(err: FrameworkError<'_, crate::SuzuData, crate::SuzuError>) {
	let log_level = framework_error_level(&err);
	log::log!(log_level, "{logged_err}",
			  logged_err = LoggedFrameworkError(&err));

	discord_report_message(&err).await;
}
