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

use std::convert::Infallible;

pub mod error;
pub mod friendly_names;
pub mod sync;

#[cfg(feature = "macros")]
pub use dyncast::implement_dyncasts;

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

#[doc(hidden)]
pub mod __ {
	#[cfg(feature = "macros")]
	pub use static_assertions::const_assert;
}
