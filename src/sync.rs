//! A threading-compatible implementation.

use crate::error::Error;
use core::any::{Any, TypeId};
use pinus::{
	prelude::*,
	sync::{PineMap, PressedPineMap},
};
use std::{
	collections::btree_map::Entry, convert::Infallible, error::Error as stdError, pin::Pin,
	sync::Arc,
};

#[cfg(feature = "macros")]
pub use crate::TypeKey;

/// Extension methods for [`Node`] and [`Arc<Node>`].
pub mod extensions {
	use std::pin::Pin;

	use super::{Arc, Node, TypeId};
	use crate::{InsertedOrExisting, NewOrExisting};
	use pinus::{
		prelude::*,
		sync::{PineMap, PressedPineMap},
	};

	/// Provides the [`.branch_for`](`BranchFor::branch_for`) method.
	pub trait BranchFor<Value, Key: Ord, Tag> {
		/// Creates a new [`Node`] instance referencing the current one.
		fn branch_for(&self, tag: Tag) -> Node<Value, Key, Tag>;
		/// Creates a new [`Node`] instance referencing the current one, without cloning this [`Arc`].
		fn into_branch_for(self, tag: Tag) -> Node<Value, Key, Tag>;
	}
	impl<Value, Key: Ord, Tag> BranchFor<Value, Key, Tag> for Arc<Node<Value, Key, Tag>> {
		fn branch_for(&self, tag: Tag) -> Node<Value, Key, Tag> {
			Arc::clone(self).into_branch_for(tag)
		}
		fn into_branch_for(self, tag: Tag) -> Node<Value, Key, Tag> {
			Node {
				parent: Some(self),
				tag,
				local_scope: PressedPineMap::new().pin(),
			}
		}
	}

	/// Provides the [`.branch_for_type`](`BranchForType::branch_for_type`) method.
	pub trait BranchForType<Value, Key: Ord>: BranchFor<Value, Key, TypeId> {
		/// Creates a new [`Node`] instance referencing the current one, tagged with the [`TypeId`] of `Tag`.
		fn branch_with_type_tag<Tag: 'static>(&self) -> Node<Value, Key, TypeId>;
		/// Creates a new [`Node`] instance referencing the current one, tagged with the [`TypeId`] of `Tag`, without cloning this [`Arc`].
		fn into_branch_for_type<Tag: 'static>(self) -> Node<Value, Key, TypeId>;
	}
	impl<Value, Key: Ord> BranchForType<Value, Key> for Arc<Node<Value, Key, TypeId>> {
		#[inline]
		fn branch_with_type_tag<Tag: 'static>(&self) -> Node<Value, Key, TypeId> {
			self.branch_for(TypeId::of::<Tag>())
		}
		#[inline]
		fn into_branch_for_type<Tag: 'static>(self) -> Node<Value, Key, TypeId> {
			self.into_branch_for(TypeId::of::<Tag>())
		}
	}

	/// Provides the [`.ensure_provided_here`](`EnsureProvidedHere::ensure_provided_here`) and [`.ensure_provided_here_with`](`EnsureProvidedHere::ensure_provided_here_with`) methods.
	pub trait EnsureProvidedHere<Value, Key, Tag> {
		/// Ensures a `Value` is provided exactly at this [`Node`], inserting it if necessary.
		fn ensure_provided_here_for(
			&self,
			key: Key,
			value: Value,
		) -> InsertedOrExisting<Pin<&Value>>;

		/// Provides a `Value` exactly at this [`Node`], creating it if not already present.
		fn ensure_provided_here_for_with<F: FnOnce(&Key) -> Value>(
			&self,
			key: Key,
			factory: F,
		) -> NewOrExisting<Pin<&Value>, Key, F>;
	}
	impl<Value, Key: Ord, Tag> EnsureProvidedHere<Value, Key, Tag> for Node<Value, Key, Tag> {
		fn ensure_provided_here_for(
			&self,
			key: Key,
			value: Value,
		) -> InsertedOrExisting<Pin<&Value>> {
			match self.local_scope.insert(key, value) {
				Ok(inserted) => InsertedOrExisting::Inserted(inserted),
				Err((key, value)) => InsertedOrExisting::Existing(
					self.local_scope.get(&key).expect("unreachable"),
					value,
				),
			}
		}

		fn ensure_provided_here_for_with<F: FnOnce(&Key) -> Value>(
			&self,
			key: Key,
			factory: F,
		) -> NewOrExisting<Pin<&Value>, Key, F> {
			match self.local_scope.insert_with(key, factory) {
				Ok(new) => NewOrExisting::New(new),
				Err((key, factory)) => NewOrExisting::Existing {
					existing: self.local_scope.get(&key).expect("unreachable"),
					key,
					factory,
				},
			}
		}
	}

	/// Provides the [`.ensure_provided_here`](`MutEnsureProvidedHere::ensure_provided_here`) and [`.ensure_provided_here_with`](`MutEnsureProvidedHere::ensure_provided_here_with`) methods, but better.
	///
	/// The same as [`EnsureProvidedHere`], but faster and returns [`InsertedOrExisting<&mut Value>`] or [`NewOrExisting<&mut Value>`].
	pub trait MutEnsureProvidedHere<Value, Key, Tag>: EnsureProvidedHere<Value, Key, Tag> {
		/// The same as [`EnsureProvidedHere::provide`], but faster and returns [`NewOrExisting<&mut Value>`].
		fn ensure_provided_here(&self, value: Value) -> InsertedOrExisting<&Value>;

		/// The same as [`EnsureProvidedHere::provide_with`], but faster and returns [`NewOrExisting<&mut Value>`].
		fn ensure_provided_here_with<F: FnOnce(&Key) -> Value>(
			&mut self,
			factory: F,
		) -> NewOrExisting<&mut Value, Key, F>;
	}

	/// Provides the [`.ensure_provided_here_for_type_key`](`EnsureProvidedHereForTypeKey::ensure_provided_here_for_type_key`) and [`.ensure_provided_here_for_type_key_with`](`EnsureProvidedHereForTypeKey::ensure_provided_here_for_type_key_with`) methods.
	pub trait EnsureProvidedHereForTypeKey<Value, Tag> {
		/// Ensures a `Value` is provided exactly at this [`Node`], inserting it if necessary.
		fn ensure_provided_here_for_type_key<Key: 'static>(
			&self,
			value: Value,
		) -> InsertedOrExisting<&Value>;

		/// Provides a `Value` exactly at this [`Node`], creating it if not already present.
		fn ensure_provided_here_for_type_key_with<Key: 'static, F: FnOnce() -> Value>(
			&self,
			factory: F,
		) -> NewOrExisting<&Value, Key, F>;
	}

	/// Provides the [`.ensure_provided_here_for_type_key`](`MutEnsureProvidedHereForTypeKey::ensure_provided_here_for_type_key`) and [`.ensure_provided_here_for_type_key_with`](`MutEnsureProvidedHereForTypeKey::ensure_provided_here_for_type_key_with`) methods, but better.
	///
	/// The same as [`EnsureProvidedHereForTypeKey`], but faster and returns [`InsertedOrExisting<&mut Value>`] or [`NewOrExisting<&mut Value>`].
	pub trait MutEnsureProvidedHereForTypeKey<Value, Tag>:
		EnsureProvidedHereForTypeKey<Value, Tag>
	{
		/// The same as [`EnsureProvidedHereForTypeKey::ensure_provided_here_for_type_key`], but faster and returns [`InsertedOrExisting<&mut Value>`].
		fn ensure_provided_here_for_type_key<Key: 'static>(
			&mut self,
			value: Value,
		) -> InsertedOrExisting<&mut Value>;

		/// The same as [`EnsureProvidedHereForTypeKey::ensure_provided_here_for_type_key_with`], but faster and returns [`NewOrExisting<&mut Value>`].
		fn ensure_provided_here_for_type_key_with<Key: 'static, F: FnOnce() -> Value>(
			&mut self,
			factory: F,
		) -> NewOrExisting<&mut Value, Key, F>;
	}
}

/// A thread-safe tagged inverse tree node.
pub struct Node<Value: ?Sized = dyn Any, Key: Ord = TypeId, Tag = TypeId> {
	parent: Option<Arc<Node<Value, Key, Tag>>>,
	tag: Tag,
	local_scope: Pin<PressedPineMap<Key, Value>>,
}

impl<V: ?Sized, K: Ord, T> Node<V, K, T> {
	/// Creates a new root-[`Node`] with tag `tag`.
	#[must_use]
	pub fn new(tag: T) -> Self {
		Self {
			parent: None,
			tag,
			local_scope: PressedPineMap::default().pin(),
		}
	}

	/// Stores `value` for `key` at the current [`Node`].
	///
	/// # Errors
	///
	/// Iff the local scope already contains a value for `key`.
	#[allow(clippy::missing_panics_doc)] //TODO: Validate the panics are indeed unreachable, then clean up potential panic sites.
	pub fn provide(&self, key: K, value: V) -> Result<Pin<&V>, (K, V)>
	where
		V: Sized,
	{
		self.local_scope.insert(key, value)
	}

	/// Stores `value` for `key` at the current [`Node`].
	///
	/// A bit more efficient than [`Self::provide`].
	///
	/// # Errors
	///
	/// Iff the local scope already contains a value for `key`.
	#[allow(clippy::missing_panics_doc)] //TODO: Validate the panics are indeed unreachable, then clean up potential panic sites.
	pub fn provide_mut(&mut self, key: K, value: V) -> Result<Pin<&mut V>, (K, V)>
	where
		V: Sized,
	{
		self.local_scope.insert_mut(key, value)
	}

	/// Wraps this instance in an [`Arc`].
	pub fn into_arc(self) -> Arc<Self> {
		Arc::new(self)
	}

	/// Retrieves a reference to this [`Node`]'s parent [`Node`], if available.
	pub fn parent(&self) -> Option<&Self> {
		self.parent.as_deref()
	}

	/// Retrieves a reference to this [`Node`]'s root [`Node`], which may be itself.
	pub fn root(&self) -> &Self {
		let mut this = self;
		while let Some(parent) = this.parent() {
			this = parent;
		}
		this
	}
}

impl<Value, Key: Ord> Node<Value, Key, TypeId> {
	/// Creates a new root-[`Node`] tagged with `Tag`'s [`TypeId`].
	#[inline]
	#[must_use]
	pub fn new_with_type_tag<Tag: 'static>() -> Self {
		Self::new(TypeId::of::<Tag>())
	}
}

impl<Value, Tag: PartialEq> Node<Value, TypeId, Tag> {
	/// Extracts a value from the node tree according to the given type key.
	///
	/// # Errors
	///
	/// Iff no value could be extracted.
	pub fn extract_by_type_key<Key: TypeKey<Value, Tag>>(&self) -> Result<&Value, Error> {
		Key::extract_value_from(self)
	}
}

//TODO: Set up Error enum. (with variants for missing keys and other errors).
//TRACKING: K must be clone until entry_insert/https://github.com/rust-lang/rust/issues/65225 lands.
impl<Value, Key: Clone + Ord, Tag: PartialEq> Node<Value, Key, Tag> {
	/// Extracts a value from the [`Node`] tree according to the given `key`.
	pub fn extract(&self, key: &Key) -> Result<&Value, Error> {
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
	) -> Result<&Value, Error> {
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

		// 	fn insert<E: stdError + 'static, V, K:  Ord, T>(
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

	fn extract_value_from(node: &Node<Value, TypeId, Tag>) -> Result<&Value, Error> {
		node.extract_or_try_provide(&Self::key(), Self::auto_provide)
	}
}
