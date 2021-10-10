//! A threading-compatible implementation.

use crate::UnwrapInfallible;
use core::any::TypeId;
use pinus::{prelude::*, sync::PressedPineMap};
use std::{
	borrow::{Borrow, BorrowMut},
	mem::MaybeUninit,
	pin::Pin,
	sync::Arc,
};
use tap::Pipe;
use this_is_fine::{prelude::*, Fine};

#[cfg(feature = "macros")]
pub use crate::TypeKey;

/// Extension methods for [`Node`] and [`Arc<Node>`].
pub mod extensions {
	use super::{Arc, Node, TypeId};
	use pinus::{prelude::*, sync::PressedPineMap};

	/// Provides the [`.branch_for`](`BranchFor::branch_for`) and [`.branch_for`](`BranchFor::into_branch_for`) methods.
	pub trait BranchFor<T, K: Ord, V: ?Sized> {
		/// Creates a new [`Node`] instance referencing the current one.
		fn branch_for(&self, tag: T) -> Node<T, K, V>;
		/// Creates a new [`Node`] instance referencing the current one, without cloning this [`Arc`].
		fn into_branch_for(self, tag: T) -> Node<T, K, V>;
	}
	impl<T, K: Ord, V: ?Sized> BranchFor<T, K, V> for Arc<Node<T, K, V>> {
		fn branch_for(&self, tag: T) -> Node<T, K, V> {
			Arc::clone(self).into_branch_for(tag)
		}
		fn into_branch_for(self, tag: T) -> Node<T, K, V> {
			Node {
				parent: Some(self),
				tag,
				local_scope: PressedPineMap::new().pin(),
			}
		}
	}

	/// Provides the [`.branch_with_type_tag`](`BranchWithTypeTag::branch_with_type_tag`) and [`.into_branch_for_type`](`BranchWithTypeTag::into_branch_with_type_tag`) methods.
	pub trait BranchWithTypeTag<K: Ord, V: ?Sized>: BranchFor<TypeId, K, V> {
		/// Creates a new [`Node`] instance referencing the current one, tagged with the [`TypeId`] of `Tag`.
		fn branch_with_type_tag<Tag: 'static>(&self) -> Node<TypeId, K, V>;
		/// Creates a new [`Node`] instance referencing the current one, tagged with the [`TypeId`] of `Tag`, without cloning this [`Arc`].
		fn into_branch_with_type_tag<Tag: 'static>(self) -> Node<TypeId, K, V>;
	}
	impl<K: Ord, V: ?Sized> BranchWithTypeTag<K, V> for Arc<Node<TypeId, K, V>> {
		#[inline]
		fn branch_with_type_tag<Tag: 'static>(&self) -> Node<TypeId, K, V> {
			self.branch_for(TypeId::of::<Tag>())
		}
		#[inline]
		fn into_branch_with_type_tag<Tag: 'static>(self) -> Node<TypeId, K, V> {
			self.into_branch_for(TypeId::of::<Tag>())
		}
	}
}

/// A thread-safe tagged inverse tree node.
pub struct Node<T, K: Ord, V: ?Sized> {
	parent: Option<Arc<Node<T, K, V>>>,
	tag: T,
	local_scope: Pin<PressedPineMap<K, V>>,
}

impl<T, K: Ord, V: ?Sized> Node<T, K, V> {
	/// Creates a new root-[`Node`] with tag `tag`.
	#[must_use]
	pub fn new(tag: T) -> Self {
		Self {
			parent: None,
			tag,
			local_scope: PressedPineMap::new().pin(),
		}
	}

	/// Creates a new root-[`Node`] with tag `tag` that will store values (almost) contiguously
	/// until `capacity` (in bytes that are the size of a maximally aligned buffer!) are exceeded.
	#[must_use]
	pub fn with_capacity(tag: T, capacity: usize) -> Self {
		Self {
			parent: None,
			tag,
			local_scope: PressedPineMap::with_capacity(capacity).pin(),
		}
	}

	/// Stores `value` for `key` at the current [`Node`].
	///
	/// # Errors
	///
	/// Iff the local scope already contains a value for `key`.
	pub fn insert(&self, key: K, value: V) -> Fine<Pin<&V>, (K, V)>
	where
		V: Sized,
	{
		self.local_scope.insert(key, value)
	}

	/// Stores `value` for `key` at the current [`Node`].
	///
	/// # Errors
	///
	/// Iff the local scope already contains a value for `key`.
	pub fn emplace<W>(&self, key: K, value: W) -> Fine<Pin<&V>, (K, W)>
	where
		W: BorrowMut<V>,
	{
		self.local_scope.emplace(key, value)
	}

	/// Stores `value` for `key` at the current [`Node`].
	///
	/// # Errors
	///
	/// Iff the local scope already contains a value for `key`.
	pub fn emplace_with<W, F: for<'a> FnOnce(&K, &'a mut MaybeUninit<W>) -> &'a mut V>(
		&self,
		key: K,
		value_factory: F,
	) -> Fine<Pin<&V>, (K, F)> {
		self.local_scope
			.as_unpinned()
			.emplace_with(key, value_factory)
			.map(|v| unsafe { Pin::new_unchecked(v) })
	}

	/// Stores `value` for `key` at the current [`Node`].
	///
	/// # Errors
	///
	/// Iff the local scope already contains a value for `key`.
	pub fn try_emplace_with<
		W,
		F: for<'a> FnOnce(&K, &'a mut MaybeUninit<W>) -> Result<&'a mut V, E>,
		E,
	>(
		&self,
		key: K,
		value_factory: F,
	) -> Result<Fine<Pin<&V>, (K, F)>, E> {
		self.local_scope
			.as_unpinned()
			.try_emplace_with(key, value_factory)
			.map(|inner_result| inner_result.map(|v| unsafe { Pin::new_unchecked(v) }))
	}

	/// Stores `value` for `key` at the current [`Node`].
	///
	/// # Errors
	///
	/// Iff the local scope already contains a value for `key`.
	pub fn emplace_with_pinning<
		W,
		F: for<'a> FnOnce(&K, Pin<&'a mut MaybeUninit<W>>) -> Pin<&'a mut V>,
	>(
		&self,
		key: K,
		value_factory: F,
	) -> Fine<Pin<&V>, (K, F)> {
		self.local_scope.emplace_with(key, value_factory)
	}

	/// Stores `value` for `key` at the current [`Node`].
	///
	/// # Errors
	///
	/// Iff the local scope already contains a value for `key`.
	pub fn try_emplace_with_pinning<
		W,
		F: for<'a> FnOnce(&K, Pin<&'a mut MaybeUninit<W>>) -> Result<Pin<&'a mut V>, E>,
		E,
	>(
		&self,
		key: K,
		value_factory: F,
	) -> Result<Fine<Pin<&V>, (K, F)>, E> {
		self.local_scope.try_emplace_with(key, value_factory)
	}

	/// Stores `value` for `key` at the current [`Node`].
	///
	/// A bit more efficient than [`Self::provide`].
	///
	/// # Errors
	///
	/// Iff the local scope already contains a value for `key`.
	#[allow(clippy::missing_panics_doc)] //TODO: Validate the panics are indeed unreachable, then clean up potential panic sites.
	pub fn insert_mut(&mut self, key: K, value: V) -> Fine<Pin<&mut V>, (K, V)>
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

	/// Retrieves a reference to the neared [`Node`] node tagged with `tag`.
	///
	/// This may be the current node or any of its ancestors, but no siblings at any level.
	#[must_use]
	pub fn tagged<Q>(&self, tag: &Q) -> Option<&Self>
	where
		T: Borrow<Q>,
		Q: PartialEq + ?Sized,
	{
		let mut this = self;
		loop {
			if this.tag.borrow() == tag {
				break Some(this);
			}
			this = this.parent()?;
		}
	}
}

impl<K: Ord, V: ?Sized> Node<TypeId, K, V> {
	/// Creates a new root-[`Node`] tagged with `Tag`'s [`TypeId`].
	///
	/// # Tracing
	///
	/// ## `error`
	#[must_use]
	pub fn new_with_type_tag<Tag: 'static>() -> Self {
		if tracing::Level::ERROR > tracing::level_filters::STATIC_MAX_LEVEL {}

		Self::new(TypeId::of::<Tag>())
	}

	/// Retrieves a reference to the neared [`Node`] node tagged with `Tag`'s [`TypeId`].
	///
	/// This may be the current node or any of its ancestors, but no siblings at any level.
	#[must_use]
	pub fn type_tagged<Tag: 'static>(&self) -> Option<&Self> {
		self.tagged(&TypeId::of::<Tag>())
	}
}

//TODO: Set up Error enum. (with variants for missing keys and other errors).
//TRACKING: K must be clone until entry_insert/https://github.com/rust-lang/rust/issues/65225 lands.
impl<T, K: Ord, V: ?Sized> Node<T, K, V> {
	/// Extracts a value from this [`Node`] only according to the given `key`.
	///
	/// # Panics
	///
	/// Iff this [`Node`] is poisoned.
	#[must_use]
	pub fn get_local<Q>(&self, key: &Q) -> Option<Pin<&V>>
	where
		K: Borrow<Q>,
		Q: Ord + ?Sized,
	{
		self.local_scope.get(key)
	}

	/// Extracts a value from the [`Node`] tree according to the given `key`.
	#[must_use]
	pub fn get<Q>(&self, key: &Q) -> Option<(&Self, Pin<&V>)>
	where
		K: Borrow<Q>,
		Q: Ord + ?Sized,
	{
		self.find(|node| node.get_local(key))
	}

	/// Extracts a value from the node tree according to the given `selector`.
	///
	/// The selector is called once each for this [`Node`] and its ancestors, in order of inverse age, until either:
	///
	/// - It returns [`Some`].
	/// - No further parent [`Node`] is available.
	///
	/// # Panics
	///
	/// Not directly, but if a poisoned [`Node`] is reached, then `selector` is likely to panic.
	#[must_use]
	pub fn find<S: Fn(&Node<T, K, V>) -> Option<Pin<&V>>>(
		&self,
		local_selector: S,
	) -> Option<(&Self, Pin<&V>)> {
		self.try_find(|node| Ok(local_selector(node)))
			.unwrap_infallible()
	}

	/// Extracts a value from the node tree according to the given `selector`.
	///
	/// The selector is called once each for this [`Node`] and its ancestors, in order of inverse age, until either:
	///
	/// - It fails.
	/// - It returns [`Some`].
	/// - No further parent [`Node`] is available.
	///
	/// # Errors
	///
	/// Iff the selector fails.
	///
	/// # Panics
	///
	/// Not directly, but if a poisoned [`Node`] is reached, then `selector` is likely to panic.
	pub fn try_find<S: Fn(&Node<T, K, V>) -> Result<Option<Pin<&V>>, E>, E>(
		&self,
		local_selector: S,
	) -> Result<Option<(&Self, Pin<&V>)>, E> {
		let mut this = self;
		loop {
			if let Some(found) = local_selector(this)? {
				break Some((this, found));
			} else if let Some(parent) = this.parent() {
				this = parent
			} else {
				break None;
			}
		}
		.pipe(Ok)
	}

	//TODO: `search` and `try_search`, which should iterate using repeat `find` and/or `try_find` calls.
}
