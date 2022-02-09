//! A threading-compatible implementation.

mod injection;
mod node;

pub use injection::{
	derive_inject_for_trait, derive_trait_dependency, BlanketSizedDependency, BlanketSizedInject,
	DynValue, Extract, Inject, RefExtract, RefExtracted,
};
pub use node::{Node, NodeHandle};
