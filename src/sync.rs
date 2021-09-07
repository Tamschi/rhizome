//! A threading-compatible implementation.

use crate::error::Error;
use core::{
	any::{Any, TypeId},
	hash::Hash,
};
use mapped_guard::{MapGuard as _, MappedGuard};
use std::{
	collections::{hash_map::Entry, HashMap},
	convert::Infallible,
	error::Error as stdError,
	sync::{Arc, RwLock, RwLockReadGuard},
};

/// Extension methods for [`Arc<Node>`].
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

/// A thread-safe tagged inverse tree node.
pub struct Node<Value = Box<dyn Any>, Key = TypeId, Tag = TypeId> {
	parent: Option<Arc<Node<Value, Key, Tag>>>,
	tag: Tag,
	local_scope: RwLock<HashMap<Key, Value>>,
}

impl<V, K: Eq + Hash, T> Node<V, K, T> {
	/// Creates a new root-[`Node`] with tag `tag`.
	#[must_use]
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

	/// Wraps this instance in an [`Arc`].
	pub fn into_arc(self) -> Arc<Self> {
		Arc::new(self)
	}

	/// Retrieves a reference to this [`Node`]'s parent [`Node`], if available.
	pub fn parent(&self) -> Option<&Self> {
		self.parent.as_deref()
	}
}

impl<V, K: Eq + Hash> Node<V, K, TypeId> {
	/// Creates a new root-[`Node`] tagged with `Tag`'s [`TypeId`].
	#[inline]
	#[must_use]
	pub fn new_for_type<Tag: 'static>() -> Self {
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
	pub fn extract_with_type_key<Key: TypeKey<V, T>>(
		&self,
	) -> Result<Mapped<'_, V, TypeId>, Error> {
		Key::extract_value_from(self)
	}
}

//TODO: Set up Error enum. (with variants for missing keys and other errors).
//TRACKING: K must be clone until entry_insert/https://github.com/rust-lang/rust/issues/65225 lands.
impl<Value, Key: Clone + Eq + Hash, Tag: PartialEq> Node<Value, Key, Tag> {
	/// Extracts a value from the [`Node`] tree according to the given `key`.
	pub fn extract(&self, key: &Key) -> Result<Mapped<'_, Value, Key>, Error> {
		self.extract_or_try_provide(key, |_| -> Result<_, Infallible> { Ok(None) })
	}

	/// Extracts a value from the node tree according to the given `key` and `provision`.
	///
	/// # Errors
	///
	/// Iff no value could be extracted.
	#[allow(clippy::missing_panics_doc)] // TODO: Validate this isn't possible, then clean up potential panic sites.
	pub fn extract_or_try_provide<E: stdError + 'static>(
		&self,
		key: &Key,
		auto_provide: impl FnOnce(&Node<Value, TypeId, Tag>) -> Result<Option<&Value>, E>,
	) -> Result<Mapped<'_, Value, Key>, Error> {
		#![allow(clippy::items_after_statements)]

		todo!();

		// let mut current = self;
		// loop {
		// 	{
		// 		if let Some(guard) = current
		// 			.local_scope
		// 			.read()
		// 			.unwrap()
		// 			.maybe_map_guard(|local_scope| local_scope.get(key))
		// 		{
		// 			return Ok(guard);
		// 		}
		// 	}

		// 	fn insert<E: stdError + 'static, V, K: Eq + Hash, T>(
		// 		node: &Node<V, K, T>,
		// 		key: K,
		// 		factory: Factory<E, V, K, T>,
		// 	) -> Result<(), Error> {
		// 		let value = factory(node).map_err(|error| Error::Other(Box::new(error)))?;
		// 		assert!(node
		// 			.local_scope
		// 			.write()
		// 			.unwrap()
		// 			.insert(key, value)
		// 			.is_none());
		// 		Ok(())
		// 	}

		// 	match (&provision, &current.parent) {
		// 		(Provision::At(Location::Tagged(tag), factory), _) if self.tag == *tag => {
		// 			insert(current, key.clone(), *factory)?;
		// 		}
		// 		(_, Some(parent)) => current = parent,
		// 		(Provision::At(Location::Root, factory), None) => {
		// 			insert(current, key.clone(), *factory)?;
		// 		}
		// 		(Provision::Never, None) => return Err(Error::NoDefault),
		// 		(Provision::At(_, _), None) => return Err(Error::NoTagMatched),
		// 	}
		// }
	}
}

pub trait TypeKey<Value = Box<dyn Any>, Tag: PartialEq = TypeId>
where
	Self: 'static,
{
	#[must_use]
	fn key() -> TypeId {
		TypeId::of::<Self>()
	}

	type Error: stdError + 'static;

	fn auto_provide(
		_queried_node: &Node<Value, TypeId, Tag>,
	) -> Result<Option<&Value>, Self::Error> {
		Ok(None)
	}

	fn extract_value_from(
		node: &Node<Value, TypeId, Tag>,
	) -> Result<Mapped<'_, Value, TypeId>, Error> {
		node.extract_or_try_provide(&Self::key(), Self::auto_provide)
	}
}
