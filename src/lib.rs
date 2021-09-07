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

#[cfg(feature = "macros")]
pub use rhizome_proc_macro_definitions::extractable;

use crate::error::Error;
use core::{
	any::{Any, TypeId},
	hash::Hash,
};
use mapped_guard::{MapGuard as _, MappedGuard};
use std::{
	collections::{hash_map::Entry, HashMap},
	error::Error as stdError,
	sync::{Arc, RwLock, RwLockReadGuard},
};

/// Extension methods for [`Arc<Node<Value, Key, Tag>>`].
pub mod extensions {
	use std::sync::RwLock;

	use super::{Arc, Hash, Node, TypeId};

	/// Provides the [`.branch_for`](`BranchFor::branch_for`) method.
	pub trait BranchFor<Value, Key, Tag> {
		/// Creates a new [`Node`] instance referencing the current one.
		fn branch_for(&self, tag: Tag) -> Node<Value, Key, Tag>;
		/// Creates a new [`Node`] instance referencing the current one, without cloning this [`Arc`].
		fn into_branch_for(self, tag: Tag) -> Node<Value, Key, Tag>;
	}
	impl<Value, Key: Eq + Hash, Tag> BranchFor<Value, Key, Tag> for Arc<Node<Value, Key, Tag>> {
		fn branch_for(&self, tag: Tag) -> Node<Value, Key, Tag> {
			Arc::clone(self).into_branch_for(tag)
		}
		fn into_branch_for(self, tag: Tag) -> Node<Value, Key, Tag> {
			Node {
				parent: Some(self),
				tag,
				local_scope: RwLock::default(),
			}
		}
	}

	/// Provides the [`.branch_for_type`](`BranchForType::branch_for_type`) method.
	pub trait BranchForType<Value, Key> {
		/// Creates a new [`Node`] instance referencing the current one, tagged with the [`TypeId`] of `Tag`.
		fn branch_for_type<Tag: 'static>(&self) -> Node<Value, Key, TypeId>;
		/// Creates a new [`Node`] instance referencing the current one, tagged with the [`TypeId`] of `Tag`, without cloning this [`Arc`].
		fn into_branch_for_type<Tag: 'static>(self) -> Node<Value, Key, TypeId>;
	}
	impl<Value, Key: Eq + Hash> BranchForType<Value, Key> for Arc<Node<Value, Key, TypeId>> {
		#[inline]
		fn branch_for_type<Tag: 'static>(&self) -> Node<Value, Key, TypeId> {
			self.branch_for(TypeId::of::<Tag>())
		}
		#[inline]
		fn into_branch_for_type<Tag: 'static>(self) -> Node<Value, Key, TypeId> {
			self.into_branch_for(TypeId::of::<Tag>())
		}
	}
}

pub struct Node<Value = Box<dyn Any>, Key = TypeId, Tag = TypeId> {
	parent: Option<Arc<Node<Value, Key, Tag>>>,
	tag: Tag,
	local_scope: RwLock<HashMap<Key, Value>>,
}

impl<V, K: Eq + Hash, T> Node<V, K, T> {
	pub fn new(tag: T) -> Self {
		Self {
			parent: None,
			tag,
			local_scope: RwLock::default(),
		}
	}

	/// Stores `value` for `key` at the current [`Node`].
	///
	/// # Errors
	///
	/// Iff the local scope already contains a value for `key`.
	#[allow(clippy::missing_panics_doc)] //TODO: Validate the panics are indeed unreachable, then clean up potential panic sites.
	pub fn provide(&mut self, key: K, value: V) -> Result<(), V> {
		let mut lock = self.local_scope.write().unwrap();
		if let Entry::Vacant(e) = lock.entry(key) {
			e.insert(value);
			Ok(())
		} else {
			Err(value)
		}
	}

	pub fn into_arc(self) -> Arc<Self> {
		Arc::new(self)
	}

	pub fn parent(&self) -> Option<&Self> {
		self.parent.as_deref()
	}
}

impl<V, K: Eq + Hash> Node<V, K, TypeId> {
	#[inline]
	#[must_use]
	pub fn new_for<Tag: 'static>() -> Self {
		Self::new(TypeId::of::<Tag>())
	}
}

type Mapped<'a, V, K> = MappedGuard<Box<RwLockReadGuard<'a, HashMap<K, V>>>, &'a V>;
impl<V, T: PartialEq> Node<V, TypeId, T> {
	/// Extracts a value from the node tree according to the given type key.
	///
	/// # Errors
	///
	/// Iff no value could be extracted.
	pub fn extract_with<Key: TypeKey<E, V, T>, E: stdError + 'static>(
		&self,
	) -> Result<Mapped<'_, V, TypeId>, Error> {
		self.extract(&Key::key(), &Key::provision())
	}
}

//TODO: Set up Error enum. (with variants for missing keys and other errors).
//TRACKING: K must be clone until entry_insert/https://github.com/rust-lang/rust/issues/65225 lands.
impl<V, K: Clone + Eq + Hash, T: PartialEq> Node<V, K, T> {
	/// Extracts a value from the node tree according to the given `key` and `provision`.
	///
	/// # Errors
	///
	/// Iff no value could be extracted.
	#[allow(clippy::missing_panics_doc)] // TODO: Validate this isn't possible, then clean up potential panic sites.
	pub fn extract<E: stdError + 'static>(
		&self,
		key: &K,
		provision: &Provision<E, V, K, T>,
	) -> Result<Mapped<'_, V, K>, Error> {
		#![allow(clippy::items_after_statements)]

		let mut current = self;
		loop {
			{
				if let Some(guard) = current
					.local_scope
					.read()
					.unwrap()
					.maybe_map_guard(|local_scope| local_scope.get(key))
				{
					return Ok(guard);
				}
			}

			fn insert<E: stdError + 'static, V, K: Eq + Hash, T>(
				node: &Node<V, K, T>,
				key: K,
				factory: Factory<E, V, K, T>,
			) -> Result<(), Error> {
				let value = factory(node).map_err(|error| Error::Other(Box::new(error)))?;
				assert!(node
					.local_scope
					.write()
					.unwrap()
					.insert(key, value)
					.is_none());
				Ok(())
			}

			match (&provision, &current.parent) {
				(Provision::At(Location::Tagged(tag), factory), _) if self.tag == *tag => {
					insert(current, key.clone(), *factory)?;
				}
				(_, Some(parent)) => current = parent,
				(Provision::At(Location::Root, factory), None) => {
					insert(current, key.clone(), *factory)?;
				}
				(Provision::Never, None) => return Err(Error::NoDefault),
				(Provision::At(_, _), None) => return Err(Error::NoTagMatched),
			}
		}
	}
}

pub trait TypeKey<E, V = Box<dyn Any>, T = TypeId>
where
	Self: 'static,
{
	#[must_use]
	fn key() -> TypeId {
		TypeId::of::<Self>()
	}

	#[must_use]
	fn provision() -> Provision<E, V, TypeId, T> {
		Provision::Never
	}
}

pub enum Provision<E, V, K = TypeId, T = TypeId> {
	Never,
	At(Location<T>, Factory<E, V, K, T>),
}

impl<E, V, K, T> Provision<E, V, K, T> {
	pub fn at_root(factory: Factory<E, V, K, T>) -> Self {
		Self::At(Location::Root, factory)
	}
}

impl<E, V, K> Provision<E, V, K, TypeId> {
	pub fn at_owner<Tag: 'static>(factory: Factory<E, V, K, TypeId>) -> Self {
		Self::At(Location::Tagged(TypeId::of::<Tag>()), factory)
	}
}

pub type Factory<E, V, K, T> = fn(&Node<V, K, T>) -> Result<V, E>;

pub enum Location<T> {
	Root,
	Tagged(T),
}
