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
pub mod friendly_names;

use std::{
	convert::Infallible,
	ops::{Deref, DerefMut},
};

#[cfg(feature = "macros")]
pub use rhizome_proc_macro_definitions::{extractable, implement_type_keys, TypeKey};

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
