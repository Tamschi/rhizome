//! A threading-compatible implementation.

mod injection;
mod node;

pub use injection::{
	derive_dependency, derive_inject, BlanketSizedDependency, BlanketSizedInject, DynValue,
	Extract, Inject, RefExtract, RefExtracted,
};
pub use node::{Node, NodeHandle};
