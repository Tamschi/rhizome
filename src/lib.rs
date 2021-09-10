//! Convenient hierarchical dependency-extraction containers, implemented as reverse trees.
//!
//! This is a data structure that can be used to implement an inversion of control pattern and a Rust-compatible equivalent of dependency-injection,
//! to enable lazy resource provision, resource shadowing and testing/configuration use cases.

#![forbid(unsafe_code)]
#![doc(html_root_url = "https://docs.rs/rhizome/0.0.1")]
#![warn(clippy::pedantic, missing_docs)]

#[cfg(doctest)]
pub mod readme {
	doc_comment::doctest!("../README.md");
}

pub mod error;
pub mod sync;

use std::ops::{Deref, DerefMut};

#[cfg(feature = "macros")]
pub use rhizome_proc_macro_definitions::{extractable, implement_type_keys, TypeKey};

/// A double-success result where the returned value may have been present already.
pub enum NewOrExisting<T, Key, F> {
	/// Indicates that the value was newly put in place.
	New(T),
	/// Indicates that the value was present already.
	Existing { existing: T, key: Key, factory: F },
}
impl<T, K, F> NewOrExisting<T, K, F> {
	/// Converts this [`NewOrExisting`] instance into a [`Result`].
	///
	/// # Errors
	///
	/// Iff this was [`Existing`](`NewOrExisting::Existing`),
	/// then [`Err((existing, key, factory))`] is returned.
	pub fn into_ok_iff_new(self) -> Result<T, (T, K, F)> {
		match self {
			NewOrExisting::New(t) => Ok(t),
			NewOrExisting::Existing {
				existing,
				key,
				factory,
			} => Err((existing, key, factory)),
		}
	}
}
impl<T, Key, F> Deref for NewOrExisting<T, Key, F>
where
	T: Deref,
{
	type Target = T::Target;

	fn deref(&self) -> &Self::Target {
		match self {
			NewOrExisting::New(t) | NewOrExisting::Existing { existing: t, .. } => t,
		}
	}
}
impl<T: DerefMut, Key, F> DerefMut for NewOrExisting<T, Key, F> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		match self {
			NewOrExisting::New(t) | NewOrExisting::Existing { existing: t, .. } => t,
		}
	}
}

/// A double-success result where the returned value may have been present already.
pub enum InsertedOrExisting<T: Deref>
where
	T::Target: Sized,
{
	/// Indicates that the value was newly put in place.
	Inserted(T),
	/// Indicates that the value was present already, and also returns the unused alternative.
	Existing(T, T::Target),
}
impl<T: Deref> InsertedOrExisting<T>
where
	T::Target: Sized,
{
	/// Converts this [`InsertedOrExisting`] instance into a [`Result`].
	///
	/// # Errors
	///
	/// Iff this was [`Existing`](`NewOrExisting::Existing`).
	pub fn into_ok_iff_new(self) -> Result<T, (T, T::Target)> {
		match self {
			InsertedOrExisting::Inserted(t) => Ok(t),
			InsertedOrExisting::Existing(t, u) => Err((t, u)),
		}
	}

	/// Unwraps the unused value if available.
	pub fn into_unused(self) -> Option<T::Target> {
		match self {
			InsertedOrExisting::Inserted(_) => None,
			InsertedOrExisting::Existing(_, u) => Some(u),
		}
	}
}
impl<T: Deref> Deref for InsertedOrExisting<T>
where
	T::Target: Sized,
{
	type Target = T::Target;

	fn deref(&self) -> &Self::Target {
		match self {
			InsertedOrExisting::Inserted(t) | InsertedOrExisting::Existing(t, _) => t,
		}
	}
}
impl<T: DerefMut> DerefMut for InsertedOrExisting<T>
where
	T::Target: Sized,
{
	fn deref_mut(&mut self) -> &mut Self::Target {
		match self {
			InsertedOrExisting::Inserted(t) | InsertedOrExisting::Existing(t, _) => t,
		}
	}
}
