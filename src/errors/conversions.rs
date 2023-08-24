use super::{Error, InternalError, Context, OptError, WithContext, Contextualizable, TwilightGatewayError, TwilightError, TwilightValidationError};
use crate::pg;
use twilight_gateway::error as twgate;
use twilight_validate as twvalid;

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

macro_rules! convert_suberror {
    ($from:ty, $to:ty, $f:expr) => {
		conversions! {
			(err: $from) -> $to {
				($f)(err)
			}
			
			(err: $from) -> Error {
				($f)(err).into()
			}
		}
    };
}
convert_suberror!(TwilightGatewayError, TwilightError, TwilightError::Gateway);
convert_suberror!(twgate::CompressionError, TwilightGatewayError, TwilightGatewayError::CompressionError);
convert_suberror!(twgate::ProcessError, TwilightGatewayError, TwilightGatewayError::ProcessError);
convert_suberror!(twgate::ReceiveMessageError, TwilightGatewayError, TwilightGatewayError::ReceiveMessageError);
convert_suberror!(twgate::SendError, TwilightGatewayError, TwilightGatewayError::SendError);

convert_suberror!(TwilightValidationError, TwilightError, TwilightError::Validation);
convert_suberror!(twvalid::channel::ChannelValidationError, TwilightValidationError, TwilightValidationError::Channel);
convert_suberror!(twvalid::command::CommandValidationError, TwilightValidationError, TwilightValidationError::Command);
convert_suberror!(twvalid::component::ComponentValidationError, TwilightValidationError, TwilightValidationError::Component);
convert_suberror!(twvalid::embed::EmbedValidationError, TwilightValidationError, TwilightValidationError::Embed);
convert_suberror!(twvalid::message::MessageValidationError, TwilightValidationError, TwilightValidationError::Message);
convert_suberror!(twvalid::request::ValidationError, TwilightValidationError, TwilightValidationError::Request);
convert_suberror!(twvalid::sticker::StickerValidationError, TwilightValidationError, TwilightValidationError::Sticker);

convert_suberror!(twilight_http::Error, TwilightError, TwilightError::Http);
convert_suberror!(twilight_util::link::webhook::WebhookParseError, TwilightError, TwilightError::WebhookParse);


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

	(twilighterr: TwilightError) -> Error {
		Error::Internal(InternalError::TwilightError(twilighterr))
	}

	(interr: InternalError) -> Error {
		Error::Internal(interr)
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

