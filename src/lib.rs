#![forbid(unsafe_code)]
#![doc(html_root_url = "https://docs.rs/rhizome/0.0.1")]
#![warn(clippy::pedantic)]

use self::error::Error;
use core::{
	any::{Any, TypeId},
	hash::Hash,
};
use mapped_guard::{MapGuard as _, MappedGuard};
use std::{
	collections::HashMap,
	error::Error as stdError,
	sync::{Arc, RwLock, RwLockReadGuard},
};

#[cfg(doctest)]
pub mod readme {
	doc_comment::doctest!("../README.md");
}

pub mod error;

#[cfg(feature = "macros")]
pub use rhizome_proc_macro_definitions::extractable;

pub mod extensions {
	use std::sync::RwLock;

	use super::{Arc, Hash, Node, TypeId};

	pub trait NodeArc<V, K, T> {
		fn derive(&self, tag: T) -> Node<V, K, T>;
	}
	impl<V, K: Eq + Hash, T> NodeArc<V, K, T> for Arc<Node<V, K, T>> {
		fn derive(&self, tag: T) -> Node<V, K, T> {
			Node {
				parent: Some(self.clone()),
				tag,
				local_scope: RwLock::default(),
			}
		}
	}

	pub trait TypeTaggedNodeArc<V, K> {
		fn derive_for<Tag: 'static>(&self) -> Node<V, K, TypeId>;
	}
	impl<V, K: Eq + Hash> TypeTaggedNodeArc<V, K> for Arc<Node<V, K, TypeId>> {
		#[inline]
		fn derive_for<Tag: 'static>(&self) -> Node<V, K, TypeId> {
			self.derive(TypeId::of::<Tag>())
		}
	}
}

pub struct Node<V = Box<dyn Any>, K = TypeId, T = TypeId> {
	parent: Option<Arc<Node<V, K, T>>>,
	tag: T,
	local_scope: RwLock<HashMap<K, V>>,
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
	pub fn provide(&mut self, key: K, value: V) -> Result<(), V> {
		let mut lock = self.local_scope.write().unwrap();
		if lock.contains_key(&key) {
			Err(value)
		} else {
			assert!(lock.insert(key, value).is_none());
			Ok(())
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
					.maybe_map_guard(|local_scope| local_scope.get(&key))
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
					insert(current, key.clone(), *factory)?
				}
				(_, Some(parent)) => current = parent,
				(Provision::At(Location::Root, factory), None) => {
					insert(current, key.clone(), *factory)?
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
