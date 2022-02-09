//! A threading-compatible implementation.
//!
//! The word "node" in this documentation refers to reference-counted backing instances
//! and not to any particular consumer-visible instance.

use crate::UnwrapInfallible;
use core::{
	borrow::{Borrow, BorrowMut},
	hash::{Hash, Hasher},
	mem::MaybeUninit,
	pin::Pin,
	ptr,
};
use pinus::{prelude::*, sync::PressedPineMap};
use std::marker::PhantomPinned;
use tap::Pipe;
use this_is_fine::Fine;

use tiptoe::{Arc, ExclusivePin, IntrusivelyCountable, ManagedClone, RefCounter, TipToe};

#[cfg(feature = "macros")]
pub use crate::TypeKey;

/// Shorthand for `Pin<Arc<Node<T, K, V, C = TipToe>>>`.
pub type NodeHandle<T, K, V, C = TipToe> = Pin<Arc<Node<T, K, V, C>>>;

impl<'a, T, K: Ord, V: ?Sized, C: RefCounter> PartialEq for Node<T, K, V, C> {
	fn eq(&self, other: &Self) -> bool {
		ptr::eq(self, other)
	}
}

impl<'a, T, K: Ord, V: ?Sized, C: RefCounter> Eq for Node<T, K, V, C> {}

impl<'a, T, K: Ord, V: ?Sized, C: RefCounter> Hash for Node<T, K, V, C> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		state.write_usize(self as *const _ as usize)
	}
}

/// A reference-counted tagged inverse map tree node.
///
/// Construct this indirectly via a [`NodeHandle`] constructor.
#[pin_project::pin_project]
pub struct Node<T, K: Ord, V: ?Sized, C: RefCounter = TipToe> {
	/// This crate walks along the graph a whole lot and normally doesn't do much refcounting,
	/// so the offset version is likely a bit better here.
	///
	/// This is unbenched though. Someone might want to check.
	parent: Option<NodeHandle<T, K, V, C>>,
	tag: T,
	local_scope: Pin<PressedPineMap<K, V>>,
	#[pin]
	ref_counter: C,
	// FIXME: Remove the extra `PhantomPinned` once `RefCounter` guarantees `!Unpin`.
	#[pin]
	pinned: PhantomPinned,
}

impl<T, K: Ord, V: ?Sized, C: RefCounter> ManagedClone for Node<T, K, V, C>
where
	T: ManagedClone,
	Pin<PressedPineMap<K, V>>: ManagedClone,
	V: Clone,
	C: ManagedClone,
{
	unsafe fn managed_clone(&self) -> Self {
		// SAFETY: Everything is wrapped as before.
		Self {
			parent: Option::managed_clone(&self.parent),
			tag: self.tag.managed_clone(),
			local_scope: self.local_scope.managed_clone(),
			ref_counter: self.ref_counter.managed_clone(),
			pinned: self.pinned.managed_clone(),
		}
	}
}

unsafe impl<T, K: Ord, V: ?Sized, C: RefCounter> IntrusivelyCountable for Node<T, K, V, C> {
	type RefCounter = C;

	fn ref_counter(&self) -> &Self::RefCounter {
		&self.ref_counter
	}
}

impl<T, K: Ord, V: ?Sized, C: RefCounter> Node<T, K, V, C> {
	/// Creates a new root-node with tag `tag`.
	#[must_use]
	pub fn new(tag: T) -> NodeHandle<T, K, V, C>
	where
		C: Default,
	{
		Self {
			parent: None,
			tag,
			local_scope: PressedPineMap::new().pin(),
			ref_counter: C::default(),
			pinned: PhantomPinned,
		}
		.pipe(Arc::pin)
	}

	/// Creates a new root-node with tag `tag` that will store values (almost) contiguously
	/// until `capacity` (in bytes that are the size of a maximally aligned buffer!) are exceeded.
	#[must_use]
	pub fn with_capacity(tag: T, capacity_bytes: usize) -> NodeHandle<T, K, V, C>
	where
		C: Default,
	{
		Self {
			parent: None,
			tag,
			local_scope: PressedPineMap::with_capacity(capacity_bytes).pin(),
			ref_counter: C::default(),
			pinned: PhantomPinned,
		}
		.pipe(Arc::pin)
	}

	/// Uses this node as parent of a new node tagged `tag`,
	/// without cloning the handle.
	#[must_use]
	pub fn handle_into_branch_for(parent: NodeHandle<T, K, V, C>, tag: T) -> NodeHandle<T, K, V, C>
	where
		C: Default,
	{
		Self {
			parent: Some(parent),
			tag,
			local_scope: PressedPineMap::new().pin(),
			ref_counter: C::default(),
			pinned: PhantomPinned,
		}
		.pipe(Arc::pin)
	}

	/// Uses this node as parent of a new node tagged `tag`,
	/// without cloning the handle,
	/// pre-allocating at least `capacity_bytes` of maximally-aligned contiguous memory to store values.
	#[must_use]
	pub fn handle_into_branch_for_with_capacity(
		parent: NodeHandle<T, K, V, C>,
		tag: T,
		capacity_bytes: usize,
	) -> NodeHandle<T, K, V, C>
	where
		C: Default,
	{
		Self {
			parent: Some(parent),
			tag,
			local_scope: PressedPineMap::with_capacity(capacity_bytes).pin(),
			ref_counter: C::default(),
			pinned: PhantomPinned,
		}
		.pipe(Arc::pin)
	}

	/// Creates an additional handle for this [`Node`], incrementing the internal reference count.
	#[must_use]
	pub fn clone_handle(&self) -> NodeHandle<T, K, V, C> {
		Pin::clone(unsafe { Arc::borrow_pin_from_inner_ref(&self) })
	}
}

/// # Branching
impl<T, K: Ord, V: ?Sized, C: RefCounter> Node<T, K, V, C> {
	/// [`Clone`]s this handle to use this node as parent of a new node tagged `tag`.
	#[must_use]
	pub fn branch_for(&self, tag: T) -> NodeHandle<T, K, V, C>
	where
		C: Default,
	{
		Self::handle_into_branch_for(self.clone_handle(), tag)
	}

	/// [`Clone`]s this handle to use this node as parent of a new node tagged `tag`,
	/// pre-allocating at least `capacity_bytes` of maximally-aligned contiguous memory to store values.
	#[must_use]
	pub fn branch_for_with_capacity(&self, tag: T, capacity_bytes: usize) -> NodeHandle<T, K, V, C>
	where
		C: Default,
	{
		Self::handle_into_branch_for_with_capacity(self.clone_handle(), tag, capacity_bytes)
	}
}

/// # Insertions
impl<T, K: Ord, V: ?Sized, C: RefCounter> Node<T, K, V, C> {
	/// Stores `value` for `key` at the current node.
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

	/// Stores `value` for `key` at the current node.
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

	/// Stores `value` for `key` at the current node.
	///
	/// # Errors
	///
	/// Iff the local scope already contains a value for `key`.
	pub fn emplace_with<W, F: for<'a> FnOnce(&K, &'a mut MaybeUninit<W>) -> &'a mut V>(
		&self,
		key: K,
		value_factory: F,
	) -> Fine<Pin<&V>, (K, F)> {
		self.local_scope.emplace_with_unpinned(key, value_factory)
	}

	/// Stores `value` for `key` at the current node.
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
			.try_emplace_with_unpinned(key, value_factory)
	}

	/// Stores `value` for `key` at the current node.
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

	/// Stores `value` for `key` at the current node.
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

	/// Retrieves a reference to this node's parent node, if available.
	#[must_use]
	pub fn parent(&self) -> Option<&Node<T, K, V, C>> {
		self.parent.as_deref()
	}

	/// Retrieves a reference to this node's root node, which may be itself.
	#[must_use]
	pub fn root(&self) -> &Node<T, K, V, C> {
		let mut this = self;
		while let Some(parent) = this.parent() {
			this = parent;
		}
		this
	}

	/// Retrieves a reference to the neared node node tagged with `tag`.
	///
	/// This may be the current node or any of its ancestors, but no siblings at any level.
	#[must_use]
	pub fn tagged<Q>(&self, tag: &Q) -> Option<&Node<T, K, V, C>>
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

/// # Retrieval
impl<T, K: Ord, V: ?Sized, C: RefCounter> Node<T, K, V, C> {
	/// Extracts a value from this node only according to the given `key`.
	///
	/// # Panics
	///
	/// Iff this node is poisoned.
	#[must_use]
	pub fn get_local<Q>(&self, key: &Q) -> Option<Pin<&V>>
	where
		K: Borrow<Q>,
		Q: Ord + ?Sized,
	{
		self.local_scope.get(key)
	}

	/// Extracts a value from the node tree according to the given `key`.
	///
	/// # Panics
	///
	/// Iff a poisoned node is reached.
	#[allow(clippy::type_complexity)]
	#[must_use]
	pub fn get<Q>(&self, key: &Q) -> Option<(&Node<T, K, V, C>, Pin<&V>)>
	where
		K: Borrow<Q>,
		Q: Ord + ?Sized,
	{
		self.find(|node| node.get_local(key))
	}

	/// Extracts a value from the node tree according to the given `selector`.
	///
	/// The selector is called once each for this node and its ancestors, in order of inverse age, until either:
	///
	/// - It returns [`Some`].
	/// - No further parent node is available.
	///
	/// # Panics
	///
	/// Not directly, but if a poisoned node is reached, then `selector` is likely to panic.
	#[allow(clippy::type_complexity)]
	#[must_use]
	pub fn find<S: for<'a> FnMut(&'a Node<T, K, V, C>) -> Option<Pin<&'a V>>>(
		&self,
		mut local_selector: S,
	) -> Option<(&Node<T, K, V, C>, Pin<&V>)> {
		self.try_find(|node| Ok(local_selector(node)))
			.unwrap_infallible()
	}

	/// Extracts a value from the node tree according to the given `selector`.
	///
	/// The selector is called once each for this node and its ancestors, in order of inverse age, until either:
	///
	/// - It fails.
	/// - It returns [`Some`].
	/// - No further parent node is available.
	///
	/// # Errors
	///
	/// Iff the selector fails.
	///
	/// # Panics
	///
	/// Not directly, but if a poisoned node is reached, then `selector` is likely to panic.
	#[allow(clippy::type_complexity)]
	pub fn try_find<S: for<'a> FnMut(&'a Node<T, K, V, C>) -> Result<Option<Pin<&'a V>>, E>, E>(
		&self,
		mut local_selector: S,
	) -> Result<Option<(&Node<T, K, V, C>, Pin<&V>)>, E> {
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

	/// Borrows the current [`Node`] exclusively iff there is only one reference to it.
	///
	/// This enables for example efficient batch insertions of values.
	pub fn as_mut(this: &mut NodeHandle<T, K, V, C>) -> Option<ExclusivePin<'_, Self>> {
		Arc::get_mut(this)
	}
}

/// # Exclusive Bulk Insertions
///
/// These functions can be batched without incurring locking overhead between them.
///
/// Use [`Node::as_mut`] to acquire a suitable exclusive [`Node`] reference.
impl<T, K: Ord, V: ?Sized, C: RefCounter> Node<T, K, V, C> {
	/// Stores `value` for `key` at the current node.
	///
	/// # Errors
	///
	/// Iff the local scope already contains a value for `key`.
	#[allow(clippy::missing_panics_doc)] //TODO: Validate the panics are indeed unreachable, then clean up potential panic sites.
	pub fn insert_mut(self: Pin<&mut Self>, key: K, value: V) -> Fine<Pin<&mut V>, (K, V)>
	where
		V: Sized,
	{
		self.project().local_scope.insert_mut(key, value)
	}

	/// Stores `value` for `key` at the current node.
	///
	/// # Errors
	///
	/// Iff the local scope already contains a value for `key`.
	pub fn emplace_mut<W>(self: Pin<&mut Self>, key: K, value: W) -> Fine<Pin<&mut V>, (K, W)>
	where
		W: BorrowMut<V>,
	{
		self.project().local_scope.emplace_mut(key, value)
	}

	/// Stores `value` for `key` at the current node.
	///
	/// # Errors
	///
	/// Iff the local scope already contains a value for `key`.
	pub fn emplace_with_mut<W, F: for<'a> FnOnce(&K, &'a mut MaybeUninit<W>) -> &'a mut V>(
		self: Pin<&mut Self>,
		key: K,
		value_factory: F,
	) -> Fine<Pin<&mut V>, (K, F)> {
		self.project()
			.local_scope
			.emplace_with_mut_unpinned(key, value_factory)
	}

	/// Stores `value` for `key` at the current node.
	///
	/// # Errors
	///
	/// Iff the local scope already contains a value for `key`.
	pub fn try_emplace_with_mut<
		W,
		F: for<'a> FnOnce(&K, &'a mut MaybeUninit<W>) -> Result<&'a mut V, E>,
		E,
	>(
		self: Pin<&mut Self>,
		key: K,
		value_factory: F,
	) -> Result<Fine<Pin<&mut V>, (K, F)>, E> {
		self.project()
			.local_scope
			.try_emplace_with_mut_unpinned(key, value_factory)
	}

	/// Stores `value` for `key` at the current node.
	///
	/// # Errors
	///
	/// Iff the local scope already contains a value for `key`.
	pub fn emplace_with_pinning_mut<
		W,
		F: for<'a> FnOnce(&K, Pin<&'a mut MaybeUninit<W>>) -> Pin<&'a mut V>,
	>(
		self: Pin<&mut Self>,
		key: K,
		value_factory: F,
	) -> Fine<Pin<&mut V>, (K, F)> {
		self.project()
			.local_scope
			.emplace_with_mut(key, value_factory)
	}

	/// Stores `value` for `key` at the current node.
	///
	/// # Errors
	///
	/// Iff the local scope already contains a value for `key`.
	pub fn try_emplace_with_pinning_mut<
		W,
		F: for<'a> FnOnce(&K, Pin<&'a mut MaybeUninit<W>>) -> Result<Pin<&'a mut V>, E>,
		E,
	>(
		self: Pin<&mut Self>,
		key: K,
		value_factory: F,
	) -> Result<Fine<Pin<&mut V>, (K, F)>, E> {
		self.project()
			.local_scope
			.try_emplace_with_mut(key, value_factory)
	}
}
