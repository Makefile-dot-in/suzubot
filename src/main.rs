#![feature(try_blocks)]
use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;
use anyhow::{anyhow, Context};
use itertools::Itertools;
use log::LevelFilter;
use suzubot_ng::init::Config;
use tokio::fs::read_to_string;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const NAME: &str = env!("CARGO_PKG_NAME");

const OPTIONS: &[(&str, &[&str], &str)] = &[
	("c",               &["PATH"],   "Specifies the config file."),
	("list_migrations", &[],         "Lists all migrations."),
	("migrate",         &["TARGET"], "Migrates the database for the selected profile to TARGET."),
	("init",            &[],         "Initializes the database for the selected profile."),
	("info",            &[],         "Prints database info"),
	("h",               &[],         "Prints this help message."),
];

struct HelpString<'a> {
	executable: &'a str
}

impl<'a> fmt::Display for HelpString<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let executable = self.executable;
		write!(f, "{NAME} v{VERSION}
Usage:
 {executable} [OPTIONS] [--] [PROFILE]
Options:
")?;

		let inherent_option_desc = OPTIONS.iter()
			.map(|&(param, args, _)| format!("-{param} {args_joined}", args_joined = args.iter().join(" ")))
			.collect::<Vec<_>>();
		let max_inh_option_desc = inherent_option_desc.iter().map(String::len).max().unwrap_or(0);
		for (inh_opt_desc, (_, _, descr)) in inherent_option_desc.into_iter().zip(OPTIONS) {
			write!(f, " {inh_opt_desc:<max_inh_option_desc$} {descr}\n")?;
		}
		Ok(())
	}
}

fn get_options(iter: &mut Peekable<impl Iterator<Item = String>>)
			   -> Result<HashMap<String, HashMap<&'static str, String>>, anyhow::Error> {
	let mut optmap = HashMap::new();
	while let Some(opt) = iter.next_if(|s| s.starts_with('-')) {
		if opt == "--" {
			break;
		}

		let optname = opt.strip_prefix("-").unwrap();
		let (_, args, _) = OPTIONS.iter().find(|&&(opt, _, _)| opt == optname).
			ok_or_else(|| anyhow!("unspported option: {optname}"))?;
		let optargs = args.iter()
			.enumerate()
			.map(|(idx, &name)| {
				Ok((name, iter.next().ok_or_else(|| anyhow!("-{optname}: expected {args_len} arguments, got {idx}", args_len = args.len()))?))
			})
			.collect::<Result<HashMap<&'static str, String>, anyhow::Error>>()?;
		optmap.insert(optname.to_owned(), optargs);
	}
	Ok(optmap)
}

fn parse_args() -> Result<(String, HashMap<String, HashMap<&'static str, String>>, Vec<String>), anyhow::Error> {
	let mut arg_iter = std::env::args().peekable();
	let exec_name = arg_iter.next()
		.ok_or_else(|| anyhow!("failed to get executable name (try googling for: argv[0] not present)"))?;
	let opts = get_options(arg_iter.by_ref()).context("parsing options")?;
	let positional = arg_iter.collect::<Vec<_>>();
	Ok((exec_name, opts, positional))
}

#[tokio::main]
async fn main() {
    env_logger::builder()
		.filter_level(LevelFilter::Info)
		.parse_default_env()
		.init();

	let init_res: Result<(), anyhow::Error> = try {
		let (exec_name, opt_args, position_args) = parse_args()?;

		if opt_args.contains_key("h") {
			eprintln!("{}", HelpString { executable: &exec_name });
			return;
		}

		let config_path = opt_args
			.get("c")
			.map(|m| &m["PATH"])
			.map(|p| p.as_str())
			.unwrap_or("suzu.toml");

		let config_str = read_to_string(config_path).await
			.with_context(|| format!("failed to open config ({config_path})"))?;

		let config: Config = toml::from_str(&config_str)
			.context("error parsing config")?;


		let profile_name_opt = match &position_args[..] {
			[profile] => Some(profile.as_str()),
			[] => None,
			[_, trailing@..] =>
				Err(anyhow!("trailing arguments: {trailing_str}",
							trailing_str = trailing.iter().join(" ")))?,
		};

		let profile_name = config.get_profile_name(profile_name_opt).to_owned();
		
		let profile = config.get_profile(&profile_name).context("config")?;

		if opt_args.contains_key("list_migrations") {
			suzubot_ng::migrations::list_migrations()?;
			return;
		}

		if let Some(migrate_opts) = opt_args.get("migrate") {
			let target = migrate_opts["TARGET"].parse::<u16>().context("parsing migration target")?;
			suzubot_ng::migrations::migrate_to(profile, target).await?;
			return;
		}

		if opt_args.contains_key("init") {
			suzubot_ng::migrations::initialize_db(profile).await?;
			return;
		}

		if opt_args.contains_key("info") {
			suzubot_ng::migrations::print_database_info(profile).await?;
			return;
		}

		log::info!("Starting profile {profile_name}");
		suzubot_ng::init::run(profile).await?;
	};

	if let Err(err) = init_res {
		log::error!("fatal error: {err:#}");
	}
}
