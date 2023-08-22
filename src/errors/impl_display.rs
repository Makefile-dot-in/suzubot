use std::fmt::{self, Display};
use super::{Context, InternalError, OptError, Error};

impl<T: Display> Display for OptError<T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self.into() {
			Some(err) => write!(f, "{err}"),
			None => write!(f, "no error")
		}
	}
}

impl Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Context::*;
        match self {
			Log(logctx) => write!(f, "{logctx}"),
            Webhook(webhctx) => write!(f, "{webhctx}"),
            Replication(repctx) => write!(f, "{repctx}"),
            Purge(purgectx) => write!(f, "{purgectx}"),
		}
    }
}

impl Display for InternalError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use InternalError::*;
		match self {
			SerenityError(err) => write!(f, "Serenity error: {err}"),
			DatabaseError(err) => write!(f, "Database error: {err}"),
			Bb8Error(err) => write!(f, "bb8 error: {err}"),
			InvalidByteADiscordIDFormat => write!(f, "invalid bytea discord ID format"),
		}
	}
}

impl Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Error::*;
		match self {
			MissingPermission(permissions) =>
				write!(f, "Missing permissions: {permissions}"),
			RoleNotFound =>
				write!(f, "Role not found"),
			MemberNotFound =>
				write!(f, "Member not found"),
			ChannelNotFound =>
				write!(f, "Channel not found"),
			MessageAlreadyCrossposted =>
				write!(f, "Message already crossposted"),
			CannotCrosspostMessage =>
				write!(f, "Cannot crosspost message"),
			Log(e) =>
				write!(f, "{e}"),
			Internal(_) =>
				write!(f, "Internal error"),
		}
	}
}
