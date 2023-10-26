use std::fmt::{self, Display};
use poise::serenity_prelude as ser;

pub struct Link<T: Linkable> {
	inner: T,
	data: T::Data,
}

pub trait Linkable: Sized {
	/// represents extra data needed to generate a link.
	type Data;

	/// internal method for generating the actual link.
	#[doc(hidden)]
	fn link_fmt(&self, f: &mut fmt::Formatter<'_>, data: &Self::Data) -> fmt::Result;

	fn link(self, data: Self::Data) -> Link<Self> {
		Link { inner: self, data }
	}
}

/// newtype that displays [`Option<ser::GuildId>`]s as either
/// the inner guild id in the [`Some`] case,
/// or as `@me` in the [`None`] case.
struct GuildOrAtMe(pub Option<ser::GuildId>);

impl Display for GuildOrAtMe {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self.0 {
			Some(guild_id) => write!(f, "{guild_id}"),
			None => write!(f, "@me")
		}
	}
}


impl<T: Linkable> Display for Link<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.link_fmt(f, &self.data)
    }
}

impl Linkable for ser::GuildId {
	type Data = ();
	fn link_fmt(&self, f: &mut fmt::Formatter<'_>, (): &Self::Data) -> fmt::Result {
		write!(f, "[<guild>](<https://discord.com/channels/{self}>)")
	}
}

impl Linkable for ser::ChannelId {
	type Data = Option<ser::GuildId>;
	fn link_fmt(&self, f: &mut fmt::Formatter<'_>, guild: &Self::Data) -> fmt::Result {
		write!(f, "https://discord.com/channels/{guild}/{self}",
			   guild = GuildOrAtMe(*guild))
	}
}

impl Linkable for ser::MessageId {
	type Data = (Option<ser::GuildId>, ser::ChannelId);
	fn link_fmt(&self, f: &mut fmt::Formatter<'_>, (guild, channel): &Self::Data) -> fmt::Result {
		write!(f, "https://discord.com/channels/{guild}/{channel}/{self}",
			   guild = GuildOrAtMe(*guild))
	}
}

impl Linkable for ser::Guild {
	type Data = ();
	fn link_fmt(&self, f: &mut fmt::Formatter<'_>, (): &Self::Data) -> fmt::Result {
		write!(f, "[{name}](<https://discord.com/channels/{guild}>)",
			   name = &self.name,
			   guild = self.id)
	}
}

impl Linkable for ser::Channel {
	type Data = ();
	fn link_fmt(&self, f: &mut fmt::Formatter<'_>, (): &Self::Data) -> fmt::Result {
		let guild_id = match self {
			ser::Channel::Guild(g) => Some(g.guild_id),
			_ => None
		};
		self.id().link_fmt(f, &guild_id)
	}
}
