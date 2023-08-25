use eyre::{eyre, Result};
use twilight_cache_inmemory::{InMemoryCache, ResourceType};
use twilight_gateway::{Intents, Shard, ShardId};
use twilight_http::Client as HttpClient;
use twilight_model::id::{Id, marker::UserMarker};
use twilight_standby::Standby;
use crate::errors::global_handler::on_error;
use crate::log::LogData;
use crate::webhook::WebhookExecutor;
use std::{collections::{HashSet, HashMap}, sync::Arc};
use crate::cmd_data;
use bb8::Pool;
use bb8_postgres::PostgresConnectionManager;
use crate::DatabaseTls;
use crate::log;
use crate::errors;



use serde::{Deserialize, Serialize};
use crate::{CustomCommandData, PoiseContext};

#[poise::command(prefix_command, owners_only, custom_data = "cmd_data().test_mode()")]
pub async fn register(ctx: PoiseContext<'_>) -> crate::errors::Result<()> {
	Ok(poise::builtins::register_application_commands_buttons(ctx).await?)
}


#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct Profile {
	owner_ids: HashSet<Id<UserMarker>>,
	#[serde(default = "Profile::default_bot_name")]
	bot_name: String,
	token: String,
	#[serde(default)]
	test_mode: bool,
	prefix: Option<String>,
	pub(crate) dbconnstr: String,
}

impl Profile {
	fn default_bot_name() -> String {
		"Suzu".to_owned()
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
			.ok_or_else(|| eyre!("{name}: profile not found"))
	}
}

#[derive(Debug)]
pub struct SuzuData {
	pub bot_name: String,
    pub dbconn: Pool<PostgresConnectionManager<DatabaseTls>>,
	pub logdata: log::LogData,
}

impl SuzuData {
	async fn new(profile: &Profile) -> anyhow::Result<Self> {
		Ok(SuzuData {
			bot_name: profile.bot_name.clone(),
			dbconn: {
				let manager = PostgresConnectionManager
					::new_from_stringlike(profile.dbconnstr, crate::db_tls())?;
				Pool::builder().build(manager).await?
			}, logdata: LogData::new()
		})
	}
}

#[derive(Debug, Clone)]
pub struct SuzuRuntime {
	pub shard: Arc<Shard>,
	pub http: Arc<HttpClient>,
	pub standby: Arc<Standby>,
	pub data: Arc<SuzuData>,
	pub cache: Arc<InMemoryCache>,
}

impl SuzuRuntime {
	pub async fn new(profile: &Profile) -> anyhow::Result<Self> {
		Ok(SuzuRuntime {
			shard: Arc::new(Shard::new(ShardId::ONE, profile.token.clone(), Intents::all())),
			standby: Arc::new(Standby::new()),
			http: Arc::new(HttpClient::new(profile.token.clone())),
			data: Arc::new(SuzuData::new(profile).await)?,
			cache: Arc::new(
				InMemoryCache::builder()
					.resource_types(ResourceType::all())
					.build()
			),
		})
	}

	pub async fn run(&self) -> errors::Result<()> {
		loop {
			let event = match self.shard.next_event().await {
				Ok(event) => event,
				Err(err) => {
					log::warn!("error receiving event: {err}");
				}
			}
		}
	}

}


pub async fn run(profile: Profile) -> Result<(), anyhow::Error> {
	let mut shard = Arc::new(Shard::new(ShardId::ONE, profile.token.clone(), Intents::all()));

	let http = Arc::new(HttpClient::new(profile.token));

	let cache = InMemoryCache::builder()
		.resource_types(ResourceType::all())
		.build();
	let framework = poise::Framework::builder()
		.options(poise::FrameworkOptions {
			commands: super::COMMANDS.iter()
				.map(|f| f())
				.filter(|cmd| profile.test_mode ||
						!CustomCommandData::from_command_data(cmd).test_mode)
				.collect::<Vec<_>>(),
			owners: profile.owner_ids,
			on_error: |err| Box::pin(on_error(err)),
			prefix_options: PrefixFrameworkOptions {
				prefix: profile.prefix,
				..Default::default()
			},
			..Default::default()
		})
		.token(profile.token)
		.intents(ser::GatewayIntents::all())
		.setup(move |ctx, _ready, framework| {
			Box::pin(async move {
				if !profile.test_mode {
					poise::builtins::register_globally(ctx, &framework.options().commands).await?;
				}
				Ok(crate::SuzuData {
					bot_name: profile.bot_name,
					webhexec: WebhookExecutor::new(),
					dbconn: {
						let manager = bb8_postgres::PostgresConnectionManager
							::new_from_stringlike(profile.dbconnstr, crate::db_tls())?;
						bb8::Pool::builder().build(manager).await?
					},
					logdata: LogData::new(),
				})
			})
		})
		.build()
		.await?;

	framework.start().await?;
	Ok(())
}
