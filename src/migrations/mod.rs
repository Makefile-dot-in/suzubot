use itertools::Itertools;
use regex::Regex;
use anyhow::{Context, anyhow};
use std::{fmt, io};
use std::cmp::Ordering;
use crate::pg;

const MIGRATION_SEP_REGEX: &str = r#"(?:\n|^)--# MIGRATION: (\d+) (.*?)\n(.*)"#;
const MIGRATION_NUM_QUERY: &str = "SELECT current_migration FROM suzu_table_metadata";
pub const UP_MIGRATIONS: &str = include_str!("sql/up.sql");
pub const DOWN_MIGRATIONS: &str = include_str!("sql/down.sql");


#[derive(Debug, Clone, Copy)]
struct Migration<'a> {
	target: u16,
	description: &'a str,
	sql: &'a str,
}

impl<'a> fmt::Display for Migration<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{target:0>3} {description}",
			   target = self.target,
			   description = self.description)
	}
}


fn parse_migrations(input: &'static str) -> anyhow::Result<Vec<Migration<'static>>> {
	let re = Regex::new(MIGRATION_SEP_REGEX).unwrap();

	let v: Vec<_> = re.captures_iter(input)
		.map(|captures| captures.extract())
		.map(|(_, [num, description, sql])| {
			Ok(Migration {
				target: num.parse::<u16>()
					.with_context(|| format!("could not parse {num} as a number"))?,
				description,
				sql
			})
		})
		.collect::<anyhow::Result<Vec<_>>>()?;
	v.windows(2)
		.map(|sl| {
			let [anum, bnum] = <&[Migration<'static>; 2]>::try_from(sl)
				.unwrap()
				.map(|m| m.target);
			(anum + 1 == bnum)
				.then_some(())
				.ok_or_else(|| anyhow!("non-sequential migrations: {anum} + 1 != {bnum}"))
		})
		.collect::<anyhow::Result<()>>()?;
	Ok(v)
}


async fn get_last_migration(client: &pg::Client) -> anyhow::Result<u16> {
	let row = client.query_one(MIGRATION_NUM_QUERY, &[]).await?;
	let dbint = row.try_get::<_, i32>(0)?;
	dbint.try_into()
		.map_err(|_| anyhow!("{dbint}: migration number too large. was there an alteration to suzu_table_metadata?"))
}


async fn connect(connstr: &str) -> anyhow::Result<pg::Client> {
	let (client, connection) = pg::connect(connstr, crate::db_tls()).await?;
	tokio::spawn(async move {
		if let Err(e) = connection.await {
			log::error!(target: "database", "{e}");
		}
	});
	Ok(client)
}

pub fn list_migrations() -> anyhow::Result<()> {
	let parsed_migrations = parse_migrations(UP_MIGRATIONS)
		.context("parsing up migrations")?;
	println!("Num Description");
	for migration in parsed_migrations {
		println!("{migration}");
	}
	Ok(())
}

async fn migrate<SkipF, TakeF>(
	client: &mut pg::Client,
	migration_file: &'static str,
	migration_name: &'static str,
	migration_article: &'static str,
	connstr: &str,
	target: u16,
	last_migration: u16,
	skip_while: SkipF,
	take_while: TakeF,
) -> anyhow::Result<()>
where SkipF: FnMut(&Migration<'static>) -> bool,
	  TakeF: FnMut(&Migration<'static>) -> bool {
	let confstr = format!("Yes, I want to migrate {connstr} from {target:0<3} to {last_migration:0<3}.");
	log::warn!("Performing {migration_article} {migration_name} migration may be irreversible losslessly! Making a backup is recommended.");
	log::warn!("Type `{confstr}` to proceed.");
	print!("input: ");
	io::stdin()
		.lines()
		.next()
		.transpose()
		.context("reading line for confirmation")?
		.is_some_and(|line| line == confstr)
		.then_some(())
		.ok_or_else(|| anyhow!("migration aborted"))?;
	log::info!("Performing {migration_article} {migration_name} migration ({last_migration:0<3} -> {target:0<3}))");
	let migrations = parse_migrations(migration_file)
		.context("parsing {migration_name} migrations")?
		.into_iter()
		.skip_while(skip_while)
		.take_while(take_while)
		.collect::<Vec<_>>();
	
	if !migrations.last().is_some_and(|m| m.target == target) {
		anyhow::bail!("{newer_migration:0>3}: migration not supported. Make sure the bot is up to date and try again.",
					  newer_migration = target.max(last_migration));
	}
	
	let transaction = client.transaction().await
		.context("starting a transaction")?;
	
	transaction.batch_execute(&migrations.into_iter().map(|m| m.sql).join("\n")).await
		.context("performing migrations")?;
	transaction.commit().await
		.context("committing the transaction")?;
	
	log::info!("Migration successful!");
	Ok(())
}

pub async fn migrate_to(profile: crate::init::Profile, target: u16) -> anyhow::Result<()> {
	let connstr = &profile.dbconnstr;
	let mut client = connect(&connstr).await.context("connecting to database")?;
	let last_migration = get_last_migration(&client).await.context("getting last migration")?;
	match target.cmp(&last_migration) {
		Ordering::Equal => {
			log::info!("Database already at the requested migration ({target:0>3} = {last_migration:0>3}), no migrations necessary");
		},
		Ordering::Greater => {
			migrate(
				&mut client,
				UP_MIGRATIONS,
				"up",
				"an",
				&connstr,
				target,
				last_migration,
				|m| m.target <= last_migration,
				|m| m.target <= target
			).await.context("performing an up migration")?;
		},
		Ordering::Less => {
			migrate(
				&mut client,
				DOWN_MIGRATIONS,
				"down",
				"a",
				&connstr,
				target,
				last_migration,
				|m| m.target >= last_migration,
				|m| m.target >= target
			).await.context("performing a down migration")?;
		},
	}
	Ok(())
}

pub async fn initialize_db(profile: crate::init::Profile) -> anyhow::Result<()> {
	log::info!("Initializing database {connstr}", connstr = &profile.dbconnstr);
	let mut client = connect(&profile.dbconnstr).await
		.context("connecting to database")?;
	let transaction = client.transaction().await
		.context("initializing a transaction")?;
	// the migration separators in UP_MIGRATIONS are just comments, so running the whole thing
	// will initialize the database to the newest migration.
	transaction.batch_execute(UP_MIGRATIONS).await
		.context("running initialization statements")?;
	transaction.commit().await?;
	log::info!("Database initialization successful");
	Ok(())
}

pub async fn print_database_info(profile: crate::init::Profile) -> anyhow::Result<()> {
	let client = connect(&profile.dbconnstr).await
		.context("connecting to database")?;

	let last_migration_opt = get_last_migration(&client).await
		.inspect_err(|err| log::warn!("failed to get current migration: {err}"))
		.ok();
	println!("Database: {connstr}", connstr = profile.dbconnstr);
	if let Some(last_migration) = last_migration_opt {
		println!("Current migration: {last_migration:0<3}");
	}
	Ok(())
}

