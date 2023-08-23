use std::fmt::{self, Display};
use std::error::Error as StdError;

use super::{Context, OptError};

/// adds context to any error
#[derive(Debug)]
pub struct WithContext<E: Contextualizable> {
	pub contexts: Vec<Context>,
	pub error: E,
}

impl<E: StdError + Contextualizable> StdError for WithContext<E> {}

impl<E: Display + Contextualizable> Display for WithContext<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for context in self.contexts.iter().rev() {
			write!(f, "{context}: ")?;
		}
		write!(f, "{error}", error = self.error)?;
		Ok(())
    }
}

/// maps an error to an empty [`WithContext`] so that
/// any [`Contextualizable`] `E` can be used in place of a
/// [`WithContext<E>`]
impl<E: Contextualizable> From<E> for WithContext<E> {
    fn from(value: E) -> Self {
        Self {
			contexts: Vec::new(),
			error: value
		}
    }
}

/// trait for anything that can be contextualized, i.e. have context added via
/// [`WithContext`]
pub trait Contextualizable: Sized {
	type Inner: Contextualizable;
	type Output;
	type MapOutput<T: Contextualizable>;
	fn contextualize(self, context: impl Into<Context>) -> Self::Output;
	fn map_contextualized<T>(
		self,
		f: impl FnOnce(Self::Inner) -> T
	) -> Self::MapOutput<T>
	where T: Contextualizable;
}

impl<E: Contextualizable> Contextualizable for WithContext<E> {
	type Inner = E;
	type Output = Self;
	type MapOutput<T: Contextualizable> = WithContext<T>;
	fn contextualize(mut self, context: impl Into<Context>) -> Self {
		self.contexts.push(context.into());
		self
	}

	fn map_contextualized<T>(
		self,
		f: impl FnOnce(Self::Inner) -> T
	) -> Self::MapOutput<T>
	where T: Contextualizable {
		WithContext {
			contexts: self.contexts,
			error: f(self.error),
		}
	}
}

macro_rules! impl_contextualizable_error {
	($typ:ty$(, $($generics:tt)+)?) => {
		impl $(<$($generics)+>)? Contextualizable for $typ {
			type Inner = Self;
			type Output = WithContext<Self>;
			type MapOutput<__T: Contextualizable> = __T;

			fn contextualize(self, context: impl Into<Context>) -> Self::Output {
				WithContext {
					contexts: vec![context.into()],
					error: self,
				}
			}

			fn map_contextualized<__T>(
				self,
				f: impl FnOnce(Self::Inner) -> __T
			) -> Self::MapOutput<__T>
			where __T: Contextualizable {
				f(self)
			}
		}
	}
}

pub(super) use impl_contextualizable_error;
impl_contextualizable_error!(Option<T>, T);
impl_contextualizable_error!(OptError<T>, T);

/// maps the error to its contextualized counterpart
impl<T, E: Contextualizable> Contextualizable for Result<T, E> {
	type Inner = E::Inner;
	type Output = Result<T, E::Output>;
	type MapOutput<U: Contextualizable> = Result<T, E::MapOutput<U>>;

	fn contextualize(self, context: impl Into<Context>) -> Self::Output {
		self.map_err(|e| e.contextualize(context))
	}

	fn map_contextualized<V>(
		self,
		f: impl FnOnce(Self::Inner) -> V
	) -> Self::MapOutput<V>
	where V: Contextualizable {
		self.map_err(|e| e.map_contextualized(f))
	}
}

/*
/// maps the contained value in the [`Option`] to its
/// contextualized counterpart
impl<T: Contextualizable> Contextualizable for Option<T> {
	type Inner = T::Inner;
	type Output = Option<T::Output>;
	type MapOutput<U: Contextualizable> = Option<T::MapOutput<U>>;

	fn contextualize(self, context: impl Into<Context>) -> Self::Output {
		self.map(|o| o.contextualize(context))
	}

	fn map_contextualized<V>(
		self,
		f: impl FnOnce(Self::Inner) -> V
	) -> Self::MapOutput<V>
	where V: Contextualizable {
		self.map(|o| o.map_contextualized(f))
	}

	
}

impl<T: Contextualizable> Contextualizable for OptError<T> {
	type Inner = T::Inner;
	type Output = OptError<T::Output>;
	type MapOutput<U: Contextualizable> = OptError<T::MapOutput<U>>;

	fn contextualize(self, context: impl Into<Context>) -> Self::Output {
		let OptError(inner) = self;
		OptError(inner.contextualize(context))
	}

	fn map_contextualized<V>(
		self,
		f: impl FnOnce(Self::Inner) -> V
	) -> Self::MapOutput<V>
	where V: Contextualizable {
		let OptError(inner) = self;
		OptError(inner.map_contextualized(f))
	}
}
*/
/// mfw no specialization
pub trait NonEqualTypes {}

