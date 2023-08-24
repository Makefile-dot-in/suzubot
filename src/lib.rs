#![feature(min_specialization, try_blocks, result_option_inspect, iter_array_chunks)]
use bb8::Pool;
use bb8_postgres::PostgresConnectionManager;
pub(crate) use postgres_types as pgtyp;
use webhook::WebhookExecutor;
pub(crate) use tokio_postgres as pg;
use errors::{WithContext, Error};

pub const COMMANDS: &[fn() -> poise::Command<SuzuData, SuzuError>] = &[
	purge::purge,
	log::log,
	runtime::register,
	comp_util::component_test,
	purge::message_stream_test,
];

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
	pub fn from_command_data<'a>(cmd: &'a poise::Command<SuzuData, SuzuError>) -> &'a CustomCommandData {
		cmd.custom_data.downcast_ref::<CustomCommandData>().unwrap_or(&DEF_CMD_DATA)
	}

	pub fn test_mode(self) -> Self {
		Self {
			test_mode: true,
			..self
		}
	}
}

pub use runtime::SuzuData;
type SuzuError = WithContext<Error>;
type DatabaseTls = pg::tls::NoTls;

fn db_tls() -> DatabaseTls { pg::tls::NoTls }

pub mod log;
pub mod purge;
pub mod webhook;
pub mod msgreplication;
pub mod errors;
pub mod runtime;
pub mod migrations;
mod linkable;
mod comp_util;
