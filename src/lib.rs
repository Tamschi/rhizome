//! Convenient hierarchical dependency-extraction containers, implemented as reverse trees.
//!
//! This is a data structure that can be used to implement an inversion of control pattern and a Rust-compatible equivalent of dependency-injection,
//! to enable lazy resource provision, resource shadowing and testing/configuration use cases.

#![doc(html_root_url = "https://docs.rs/rhizome/0.0.1")]
#![warn(clippy::pedantic, missing_docs)]
#![allow(clippy::semicolon_if_nothing_returned)]

#[cfg(doctest)]
pub mod readme {
	doc_comment::doctest!("../README.md");
}

pub mod error;
pub mod friendly_names;
pub mod sync;

use std::{
	convert::Infallible,
	ops::{Deref, DerefMut},
	pin::Pin,
};

#[cfg(feature = "macros")]
pub use rhizome_proc_macro_definitions::{
	extractable, implement_dyncasts, implement_type_keys, Dyncast, TypeKey,
};
use tap::Pipe;

trait UnwrapInfallible {
	type T;
	fn unwrap_infallible(self) -> Self::T;
}
impl<T> UnwrapInfallible for Result<T, Infallible> {
	type T = T;
	fn unwrap_infallible(self) -> Self::T {
		self.unwrap()
	}
}

/// Reference downcasting, also to pinned trait objects.
pub trait Dyncast {
	fn dyncast<T: 'static + ?Sized>(&self) -> Option<&T>;
	fn dyncast_mut<T: 'static + ?Sized>(&mut self) -> Option<&mut T>;
	fn dyncast_pinned<'a, T: 'static + ?Sized>(self: Pin<&'a Self>) -> Option<Pin<&'a T>>;
	fn dyncast_pinned_mut<'a, T: 'static + ?Sized>(
		self: Pin<&'a mut Self>,
	) -> Option<Pin<&'a mut T>>;
}
