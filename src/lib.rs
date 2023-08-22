#![feature(min_specialization, try_blocks, result_option_inspect, iter_array_chunks)]
use bb8::Pool;
use bb8_postgres::PostgresConnectionManager;
pub(crate) use postgres_types as pgtyp;
use webhook::WebhookExecutor;
pub(crate) use tokio_postgres as pg;
use errors::{WithContext, Error};

const APP_NAME: &'static str = "Suzu";

pub struct Data {
    webhexec: WebhookExecutor,
    dbconn: Pool<PostgresConnectionManager<pg::tls::NoTls>>,
	logdata: log::LogData,
}


type PoiseContext<'a> = poise::Context<'a, Data, WithContext<Error>>;

pub mod log;
pub mod purge;
pub mod webhook;
pub mod msgreplication;
pub mod errors;
mod linkable;
mod comp_util;
