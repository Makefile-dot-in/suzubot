use std::{
    fmt,
    ops::{Deref, DerefMut},
    str::FromStr,
    sync::Arc,
};

use crate::errors::{InternalError, Result};
use poise::serenity_prelude as ser;
use std::result::Result as StdResult;
use std::time::Duration as StdDuration;

pub fn vec_to_u64(vec: Vec<u8>) -> Result<u64> {
    let array: [u8; 8] = vec
        .try_into()
        .map_err(|_| InternalError::InvalidByteADiscordIDFormat)?;
    Ok(u64::from_be_bytes(array))
}

macro_rules! event_handlers  {
    {$($handler:path),*} => {
        async fn event_dispatcher<'a>(
			ctx: &'a ser::Context,
			evt: &'a poise::Event<'a>,
			fwctx: FrameworkContext<'a, Data, SuzuError>,
			data: &Data
		) -> errors::Result<()> {
			$(
				$handler(ctx, evt, fwctx, data).await?;
			)*
			Ok(())
		}
    };
}
use chrono::{DateTime, Utc};
pub(super) use event_handlers;

macro_rules! services {
    ($($service:path),*$(,)?) => {
        fn start_services(
            ctx: &ser::Context,
            ready: &ser::Ready,
            shardmgr: &Arc<Mutex<ser::ShardManager>>,
            data: &crate::Data
        ) {
            let current_user = Arc::new(ready.user.clone());
            $(
                tokio::spawn($service(
                    ctx.clone(),
                    Arc::clone(&current_user),
                    Arc::clone(shardmgr),
                    Arc::clone(data)
                ));
            )*
        }
    };
}

#[derive(Clone, Copy, Debug)]
pub struct ParsedDatetime(pub DateTime<Utc>);

impl FromStr for ParsedDatetime {
    type Err = DateError;

    fn from_str(s: &str) -> StdResult<Self, Self::Err> {
        let now = Utc::now();
        let dt = interim::parse_date_string(s, now, interim::Dialect::Us)?;
        Ok(ParsedDatetime(dt))
    }
}

impl Deref for ParsedDatetime {
    type Target = DateTime<Utc>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ParsedDatetime {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<ParsedDatetime> for DateTime<Utc> {
    fn from(p: ParsedDatetime) -> Self {
        p.0
    }
}

#[derive(Debug, Clone)]
pub enum GetLatencyError {
    ShardIDNotInMap,
}

impl fmt::Display for GetLatencyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use GetLatencyError::*;
        match self {
            ShardIDNotInMap => write!(f, "shard ID not in map"),
        }
    }
}

pub async fn get_latency(
    shardmgr: &Arc<Mutex<ShardManager>>,
    shardid: ser::ShardId,
) -> StdResult<Option<StdDuration>, GetLatencyError> {
    let shardmgrguard = shardmgr.lock().await;
    let shardmgrmapguard = shardmgrguard.runners.lock().await;

    match shardmgrmapguard.get(&shardid) {
        Some(shardinfo) => Ok(shardinfo.latency),
        None => Err(GetLatencyError::ShardIDNotInMap),
    }
}

use interim::DateError;
use poise::serenity_prelude::ShardManager;
pub(super) use services;
use tokio::sync::Mutex;
