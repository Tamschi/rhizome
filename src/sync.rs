//! A threading-compatible implementation.

mod injection;
mod node;

pub use injection::{
	derive_trait_dependency, derive_trait_injectable, BlanketSizedDependency,
	BlanketSizedInjectable, DynValue, Extractable, Injectable, RefExtractable, RefExtracted,
};
pub use node::{Node, NodeHandle};
