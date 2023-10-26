use anyhow::anyhow;
use poise::{serenity_prelude as ser, PrefixFrameworkOptions};
use crate::{errors::global_handler::on_error, admin};
use crate::log::LogData;
use crate::webhook::WebhookExecutor;
use std::{collections::{HashSet, HashMap}, sync::Arc};
use crate::cmd_data;



use serde::{Deserialize, Serialize};
use crate::{CustomCommandData, PoiseContext};

#[poise::command(prefix_command, owners_only, custom_data = "cmd_data().test_mode()")]
pub async fn register(ctx: PoiseContext<'_>) -> crate::errors::Result<()> {
	Ok(poise::builtins::register_application_commands_buttons(ctx).await?)
}


#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Profile {
	owner_ids: HashSet<ser::UserId>,
	#[serde(default = "Profile::default_bot_name")]
	bot_name: String,
	token: String,
	#[serde(default)]
	test_mode: bool,
	prefix: Option<String>,
    #[serde(default = "Profile::default_message_cache_size")]
    message_cache_size: usize,
	pub(crate) dbconnstr: String,
}

impl Profile {
	fn default_bot_name() -> String {
		"Suzu".to_owned()
	}

    fn default_message_cache_size() -> usize {
        500
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Config {
	#[serde(default = "Config::default_default_profile")]
	default: String,
	#[serde(flatten)]
	profiles: HashMap<String, Profile>,
}

impl Config {
	fn default_default_profile() -> String {
		"default".to_owned()
	}

	pub fn get_profile_name<'a>(&'a self, name: Option<&'a str>) -> &'a str {
		name.unwrap_or(&self.default)
	}

	pub fn get_profile(mut self, name: &str) -> Result<Profile, anyhow::Error> {
		self.profiles.remove(name)
			.ok_or_else(|| anyhow!("{name}: profile not found"))
	}
}



pub async fn run(profile: Profile) -> Result<(), anyhow::Error> {
	let framework = poise::Framework::builder()
		.options(poise::FrameworkOptions {
			commands: super::COMMANDS.iter()
				.map(|f| f())
				.filter(|cmd| profile.test_mode ||
						!CustomCommandData::from_command_data(cmd).test_mode)
                .chain(admin::admin(profile.test_mode).into_iter())
				.collect::<Vec<_>>(),
			owners: profile.owner_ids,
			on_error: |err| Box::pin(on_error(err)),
            event_handler: |ctx, evt, fwctx, data|
                Box::pin(crate::event_dispatcher(ctx, evt, fwctx, data)),
			prefix_options: PrefixFrameworkOptions {
				prefix: profile.prefix,
				..Default::default()
			},
			..Default::default()
		})
		.token(profile.token)
		.intents(ser::GatewayIntents::all())
        .client_settings(move |c| {
            c.cache_settings(|s| {
                s.max_messages(profile.message_cache_size)
            })
        })
		.setup(move |ctx, ready, framework| {
			Box::pin(async move {
				if !profile.test_mode {
					poise::builtins::register_globally(ctx, &framework.options().commands).await?;
				}
				let data = Arc::new(crate::InnerData {
					bot_name: profile.bot_name,
					webhexec: WebhookExecutor::new(),
					dbconn: {
						let manager = bb8_postgres::PostgresConnectionManager
							::new_from_stringlike(profile.dbconnstr, crate::db_tls())?;
						bb8::Pool::builder().build(manager).await?
					},
					logdata: LogData::new(),
				});

                crate::start_services(
                    ctx,
                    ready,
                    framework.shard_manager(),
                    &data
                );
                Ok(data)
            })
		})
		.build()
		.await?;

	framework.start().await?;
	Ok(())
}
