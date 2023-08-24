use std::fmt::{Display};
use async_trait::async_trait;
use futures::Future;

use std::error::Error as StdError;
use poise::FrameworkError;
use poise::serenity_prelude as ser;
use std::result::Result as StdResult;

use crate::pg;

mod contextualizable;
pub use contextualizable::WithContext;
pub use contextualizable::Contextualizable;

use self::contextualizable::impl_contextualizable_error;


mod conversions;
mod impl_display;
pub(crate) mod global_handler;

pub type Result<T> = StdResult<T, WithContext<Error>>;
pub type CmdResult<T> = StdResult<T, WithContext<OptError<InternalError>>>;

#[derive(Clone, Copy, Debug)]
pub struct OptError<T>(pub Option<T>);

struct LoggedFrameworkError<'a, 'b>(&'a FrameworkError<'b, crate::Data, crate::SuzuError>);

#[derive(Debug, Clone, Copy)]
pub(crate) struct LoggedMappedWithContext<'a, E, F>(pub &'a WithContext<E>, pub fn(&'a E) -> F)
where E: Contextualizable;

#[derive(Debug, Clone, Copy)]
pub(crate) struct LoggedError<'a>(pub &'a Error);

#[derive(Debug)]
struct LoggedContext<'a>(crate::PoiseContext<'a>);

#[derive(Debug)]
pub enum Context {
	Log(crate::log::LogErrorContext),
	Webhook(crate::webhook::WebhookErrorContext),
	Replication(crate::msgreplication::ReplicationErrorContext),
	Purge(crate::purge::PurgeErrorContext),
}


#[derive(Debug)]
pub enum InternalError {
	SerenityError(ser::Error),
	DatabaseError(pg::Error),
	Bb8Error(bb8::RunError<pg::Error>),
	InvalidByteADiscordIDFormat
}

#[derive(Debug)]
pub enum Error {
	MissingPermission(ser::Permissions),
	RoleNotFound,
	MemberNotFound,
	ChannelNotFound,
	MessageAlreadyCrossposted,
	CannotCrosspostMessage,

	Log(crate::log::LogError),
	Internal(InternalError)
}

impl StdError for Error {}

impl_contextualizable_error!(InternalError);
impl_contextualizable_error!(Error);

#[async_trait]
pub trait AsyncReportErr {
	async fn report_err<F, Fut>(self, inspector: F) -> Self
	where F: FnOnce(String) -> Fut + Send,
		  Fut: Future + Send;
}

#[async_trait]
impl<T, E> AsyncReportErr for StdResult<T, E>
where T: Send + Sync,
	  E: Send + Sync + Display + 'static {
	async fn report_err<F, Fut>(self, inspector: F) -> Self
	where F: FnOnce(String) -> Fut + Send,
		  Fut: Future + Send {
		if let Err(e) = &self {
			inspector(format!("Error: {e}")).await;
		}
		self
	}
}

pub trait LogError {
	fn logerr(self);
}

impl<T, E: Display> LogError for StdResult<T, E> {
	fn logerr(self) {
		if let Err(e) = self {
			log::info!("{e}");
		}
	}
}
