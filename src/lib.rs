//! Convenient hierarchical dependency-extraction containers, implemented as reverse trees.
//!
//! [![Zulip Chat](https://img.shields.io/endpoint?label=chat&url=https%3A%2F%2Fiteration-square-automation.schichler.dev%2F.netlify%2Ffunctions%2Fstream_subscribers_shield%3Fstream%3Dproject%252Frhizome)](https://iteration-square.schichler.dev/#narrow/stream/project.2Frhizome)
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

use core::convert::Infallible;

pub mod sync;

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
	pub use fruit_salad;
	pub use this_is_fine;
	pub use tiptoe;
}
