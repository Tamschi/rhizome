//! A threading-compatible implementation.
//!
//! The word "node" in this documentation refers to reference-counted backing instances
//! and not to any particular consumer-visible instance.

mod node;

pub use node::{Node, NodeHandle};
