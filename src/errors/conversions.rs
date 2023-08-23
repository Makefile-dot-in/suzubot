use super::{Error, InternalError, Context, OptError, WithContext, Contextualizable};
use crate::pg;

use poise::{serenity_prelude as ser, FrameworkError};

macro_rules! conversions {
	// square brackets because angle brackets do not start a tree
	{ $($(for [$($generics:tt)+])? ($name:ident : $from:ty) -> $to:ty { $($body:tt)+ })+ } => {
		$(
			impl $(<$($generics)+>)? From<$from> for $to {
				fn from($name: $from) -> Self {
					$($body)+
				}
			}
			
			impl $(<$($generics)+>)? From<$from> for WithContext<$to>
			where $to: Contextualizable {
				fn from(value: $from) -> Self {
					let noctx: $to = value.into();
					noctx.into()
				}
			}
		)+
	}
}

conversions! {
	for [T] (opterr: OptError<T>) -> Option<T> {
		opterr.0
	}

	for [T] (opt: Option<T>) -> OptError<T> {
		OptError(opt)
	}

	(err: Error) -> OptError<InternalError> {
		match err {
			Error::Internal(ierr) => OptError(Some(ierr)),
			_ => OptError(None),
		}
	}

	(interr: InternalError) -> Error {
		Error::Internal(interr)
	}

	(sererr: ser::SerenityError) -> Error {
		use ser::SerenityError as Ser;
		use ser::ModelError as Model;
		match sererr {
			Ser::Model(Model::RoleNotFound) => Error::RoleNotFound,
			Ser::Model(Model::MemberNotFound) => Error::MemberNotFound,
			Ser::Model(Model::ChannelNotFound) => Error::ChannelNotFound,
			Ser::Model(Model::MessageAlreadyCrossposted) => Error::MessageAlreadyCrossposted,
			Ser::Model(Model::CannotCrosspostMessage) => Error::CannotCrosspostMessage,
			e => Error::Internal(InternalError::SerenityError(e))
		}
	}

	(pgerr: pg::Error) -> Error {
		Error::Internal(InternalError::DatabaseError(pgerr))
	}

	(bb8err: bb8::RunError<pg::Error>) -> Error {
		Error::Internal(InternalError::Bb8Error(bb8err))
	}

	(logerr: crate::log::LogError) -> Error {
		Error::Log(logerr)
	}
}

impl From<crate::log::LogErrorContext> for Context {
	fn from(logctx: crate::log::LogErrorContext) -> Self {
		Context::Log(logctx)
	}
}

impl From<crate::webhook::WebhookErrorContext> for Context {
	fn from(webhctx: crate::webhook::WebhookErrorContext) -> Self {
		Context::Webhook(webhctx)
	}
}

impl From<crate::msgreplication::ReplicationErrorContext> for Context {
	fn from(replctx: crate::msgreplication::ReplicationErrorContext) -> Self {
		Context::Replication(replctx)
	}
}

impl From<crate::purge::PurgeErrorContext> for Context {
    fn from(purgectx: crate::purge::PurgeErrorContext) -> Self {
        Context::Purge(purgectx)
    }
}

