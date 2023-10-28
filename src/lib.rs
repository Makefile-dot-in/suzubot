#![feature(min_specialization, try_blocks, result_option_inspect, iter_array_chunks, int_roundings)]
use bb8::Pool;
use bb8_postgres::PostgresConnectionManager;
use chrono::Datelike;
pub(crate) use postgres_types as pgtyp;
use webhook::WebhookExecutor;
pub(crate) use tokio_postgres as pg;
use std::{fmt, ops::Range, sync::Arc};
use errors::{WithContext, Error};
use poise::{serenity_prelude as ser, FrameworkContext};
use tokio::sync::Mutex;
use utils::{event_handlers, services};

pub const COMMANDS: &[fn() -> poise::Command<Data, SuzuError>] = &[
	purge::purge,
	init::register,
	comp_util::component_test,
    comp_util::cache_test,
	purge::message_stream_test,
    remind::remind,
	remind::reminders
];

pub const ADMIN_COMMANDS: &[fn() -> poise::Command<Data, SuzuError>] = &[
	log::log,
	remind::admin::reminders
];

event_handlers! {
	log::log_event
}

services! {
    remind::service::service
}



#[derive(Debug, Clone)]
struct CustomCommandData {
	test_mode: bool,
	__non_exhaustive: (),
}

const DEF_CMD_DATA: CustomCommandData = CustomCommandData {
	test_mode: false,
	__non_exhaustive: (),
};

fn cmd_data() -> CustomCommandData { DEF_CMD_DATA }


impl Default for CustomCommandData {
	fn default() -> Self {
		DEF_CMD_DATA
	}
}

impl CustomCommandData {
	pub fn from_command_data<'a>(cmd: &'a poise::Command<Data, SuzuError>) -> &'a CustomCommandData {
		cmd.custom_data.downcast_ref::<CustomCommandData>().unwrap_or(&DEF_CMD_DATA)
	}

	pub fn test_mode(self) -> Self {
		Self {
			test_mode: true,
			..self
		}
	}
}

/// converts the `range` of chars in `s` from chars to 
fn ctb_idx_range(s: &str, range: Range<usize>) -> Range<usize> {
	let mut indices = s.char_indices();
	let startidx = indices.by_ref()
		.skip(range.start)
		.next()
		.map(|(cidx, _)| cidx)
		.unwrap_or(s.len()); // we might as well give s.len since we're not finding anything anyways
	// if range.end > range.start, the range is empty and so the converted range would also be empty.
	// diff is how many we would skip. some example values to help verify correctness:
	// range = 0..0 => return 0..0
	// range = 0..1 => diff = 0 (we don't have to skip anything)
	// range = 0..4 => diff = 3 (we read the 0th element, skip 1, 2, 3, then read the 4th element)
	let Some(diff) = range.end.checked_sub(range.start + 1) else { return 0..0 }; 
	let endidx = indices.by_ref()
		.skip(diff)
		.next()
		.map(|(cidx, _)| cidx)
		.unwrap_or(s.len());
	startidx..endidx
}

fn truncate(mut s: String, n: usize) -> String {
	if let Some(byte_range) = Some(ctb_idx_range(&s, 0..n)).filter(|r| r.end < s.len()) {
		if n >= 10 {
			s.truncate(ctb_idx_range(&s, 0..n-3).end);
			s.push_str("...");
		} else {
			s.truncate(byte_range.end);
		}
	}
	s
}

fn fmttime_discord(t: ser::Timestamp) -> impl ToString + fmt::Display {
	t.format("<t:%s>")
}


fn ts_to_id<Id: From<u64>>(t: ser::Timestamp) -> Id {
	let discord_epoch = ser::GenericId(0).created_at();
	let time_since_epoch = *t - *discord_epoch;
	let millis_since_epoch: u64 = time_since_epoch.num_milliseconds()
		.try_into()
		.expect(&format!("are you time travelling to before {epoch_year}? if so, \
						 patch Serenity's Timestamp::created_at to reflect the actual discord epoch; \
						 otherwise, change your system clock.",
						 epoch_year = discord_epoch.year()));	
	Id::from(millis_since_epoch << 22)
}
#[derive(Debug)]
pub struct InnerData {
	bot_name: String,
    webhexec: WebhookExecutor,
    dbconn: Pool<PostgresConnectionManager<DatabaseTls>>,
	logdata: log::LogData,
}

type Data = Arc<InnerData>;
type SuzuError = WithContext<Error>;
type PoiseContext<'a> = poise::Context<'a, Data, WithContext<Error>>;
type DatabaseTls = pg::tls::NoTls;

fn db_tls() -> DatabaseTls { pg::tls::NoTls }

pub mod log;
pub mod purge;
pub mod webhook;
pub mod msgreplication;
pub mod errors;
pub mod init;
pub mod migrations;
pub mod remind;
pub mod admin;
mod utils;
mod linkable;
mod comp_util;
