//! A threading-compatible implementation.
//!
//! The word "node" in this documentation refers to reference-counted backing instances
//! and not to any particular consumer-visible instance.

use crate::UnwrapInfallible;
use core::{
	borrow::{Borrow, BorrowMut},
	hash::{Hash, Hasher},
	mem,
	mem::MaybeUninit,
	ops::Deref,
	pin::Pin,
	ptr,
};
use pinus::{prelude::*, sync::PressedPineMap};
use std::marker::PhantomPinned;
use tap::Pipe;
use this_is_fine::Fine;

use triomphe::{Arc, ArcBorrow, OffsetArc};

#[cfg(feature = "macros")]
pub use crate::TypeKey;

/// A thread-safe tagged inverse map tree node *handle*.
///
/// Note that equality is implemented as node identity.
/// A [cloned](`Clone::clone`) handle will be equal, but a [cloned](`Clone::clone`) node's handle will no be.
#[repr(transparent)]
pub struct NodeHandle<T, K: Ord, V: ?Sized> {
	inner: Arc<Node<T, K, V>>,
}

impl<T, K: Ord, V: ?Sized> Deref for NodeHandle<T, K, V> {
	type Target = Node<T, K, V>;

	fn deref(&self) -> &Self::Target {
		&self.inner
	}
}

impl<T, K: Ord, V: ?Sized> Clone for NodeHandle<T, K, V> {
	fn clone(&self) -> Self {
		Self {
			inner: self.inner.clone(),
		}
	}
}

impl<T, K: Ord, V: ?Sized> PartialEq for NodeHandle<T, K, V> {
	fn eq(&self, other: &Self) -> bool {
		ptr::eq(&*self.inner, &*other.inner)
	}
}

impl<T, K: Ord, V: ?Sized> Eq for NodeHandle<T, K, V> {}

impl<T, K: Ord, V: ?Sized> Hash for NodeHandle<T, K, V> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		state.write_usize(&*self.inner as *const _ as usize)
	}
}

impl<T, K: Ord, V: ?Sized> Borrow<Node<T, K, V>> for NodeHandle<T, K, V> {
	fn borrow(&self) -> &Node<T, K, V> {
		&self.inner
	}
}

impl<T, K: Ord, V: ?Sized> AsRef<Node<T, K, V>> for NodeHandle<T, K, V> {
	fn as_ref(&self) -> &Node<T, K, V> {
		&self.inner
	}
}

impl<'a, T, K: Ord, V: ?Sized> PartialEq for Node<T, K, V> {
	fn eq(&self, other: &Self) -> bool {
		ptr::eq(self, other)
	}
}

impl<'a, T, K: Ord, V: ?Sized> Eq for Node<T, K, V> {}

impl<'a, T, K: Ord, V: ?Sized> Hash for Node<T, K, V> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		state.write_usize(self as *const _ as usize)
	}
}

/// An (externally) reference-counted tagged inverse map tree node.
///
/// Construct this indirectly via a [`NodeHandle`] constructor.
#[pin_project::pin_project]
pub struct Node<T, K: Ord, V: ?Sized> {
	/// This crate walks along the graph a whole lot and normally doesn't do much refcounting,
	/// so the offset version is likely a bit better here.
	///
	/// This is unbenched though. Someone might want to check.
	parent: Option<OffsetArc<Node<T, K, V>>>,
	tag: T,
	local_scope: Pin<PressedPineMap<K, V>>,
	#[pin]
	_pinned: PhantomPinned,
}

impl<T, K: Ord, V: ?Sized> NodeHandle<T, K, V> {
	/// Creates a new root-node with tag `tag`.
	#[must_use]
	pub fn new(tag: T) -> Self {
		Self {
			inner: Arc::new(Node {
				parent: None,
				tag,
				local_scope: PressedPineMap::new().pin(),
				_pinned: PhantomPinned,
			}),
		}
	}

	/// Creates a new root-node with tag `tag` that will store values (almost) contiguously
	/// until `capacity` (in bytes that are the size of a maximally aligned buffer!) are exceeded.
	#[must_use]
	pub fn with_capacity(tag: T, capacity_bytes: usize) -> Self {
		Self {
			inner: Arc::new(Node {
				parent: None,
				tag,
				local_scope: PressedPineMap::with_capacity(capacity_bytes).pin(),
				_pinned: PhantomPinned,
			}),
		}
	}

	/// As long as only a single handle exists,
	/// it can be borrowed exclusively for more efficient batch modifications.
	///
	/// > Internally, this skips synchronisation steps/locking entirely.
	/// >
	/// > It's theoretically also feasible to similarly lock a shared node,
	/// > but this requires a matching lock/guard API in `pinus`.
	#[must_use]
	pub fn as_exclusive(&mut self) -> Option<Pin<&mut Node<T, K, V>>> {
		// This appears to be the nicest way to do this as of `triomphe` 0.1.3…
		Arc::get_mut(&mut self.inner).map(|exclusive| unsafe { Pin::new_unchecked(exclusive) })
	}

	/// Uses this node as parent of a new node tagged `tag`,
	/// without cloning the handle.
	#[must_use]
	pub fn into_branch_for(self, tag: T) -> Self {
		NodeHandle {
			inner: Arc::new(Node {
				parent: Some(Arc::into_raw_offset(self.inner)),
				tag,
				local_scope: PressedPineMap::new().pin(),
				_pinned: PhantomPinned,
			}),
		}
	}

	/// Uses this node as parent of a new node tagged `tag`,
	/// without cloning the handle,
	/// pre-allocating at least `capacity_bytes` of maximally-aligned contiguous memory to store values.
	#[must_use]
	pub fn into_branch_for_with_capacity(self, tag: T, capacity_bytes: usize) -> Self {
		NodeHandle {
			inner: Arc::new(Node {
				parent: Some(Arc::into_raw_offset(self.inner)),
				tag,
				local_scope: PressedPineMap::with_capacity(capacity_bytes).pin(),
				_pinned: PhantomPinned,
			}),
		}
	}
}

/// # Branching
impl<T, K: Ord, V: ?Sized> Node<T, K, V> {
	/// [`Clone`]s this handle to use this node as parent of a new node tagged `tag`.
	#[must_use]
	pub fn branch_for(&self, tag: T) -> NodeHandle<T, K, V> {
		// FIXME: UB but currently works. An intrusive `Arc` could avoid that issue.
		unsafe { mem::transmute::<&Self, ArcBorrow<'_, Self>>(self) }
			.clone_arc()
			.pipe(|inner| NodeHandle { inner })
			.into_branch_for(tag)
	}

	/// [`Clone`]s this handle to use this node as parent of a new node tagged `tag`,
	/// pre-allocating at least `capacity_bytes` of maximally-aligned contiguous memory to store values.
	#[must_use]
	pub fn branch_for_with_capacity(&self, tag: T, capacity_bytes: usize) -> NodeHandle<T, K, V> {
		// FIXME: UB but currently works. An intrusive `Arc` could avoid that issue.
		unsafe { mem::transmute::<&Self, ArcBorrow<'_, Self>>(self) }
			.clone_arc()
			.pipe(|inner| NodeHandle { inner })
			.into_branch_for_with_capacity(tag, capacity_bytes)
	}
}

/// # Insertions
impl<T, K: Ord, V: ?Sized> Node<T, K, V> {
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
	pub fn parent(&self) -> Option<&Node<T, K, V>> {
		self.parent.as_deref()
	}

	/// Retrieves a reference to this node's root node, which may be itself.
	#[must_use]
	pub fn root(&self) -> &Node<T, K, V> {
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
	pub fn tagged<Q>(&self, tag: &Q) -> Option<&Node<T, K, V>>
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
impl<T, K: Ord, V: ?Sized> Node<T, K, V> {
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
	pub fn get<Q>(&self, key: &Q) -> Option<(&Node<T, K, V>, Pin<&V>)>
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
	pub fn find<S: for<'a> FnMut(&'a Node<T, K, V>) -> Option<Pin<&'a V>>>(
		&self,
		mut local_selector: S,
	) -> Option<(&Node<T, K, V>, Pin<&V>)> {
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
	pub fn try_find<S: for<'a> FnMut(&'a Node<T, K, V>) -> Result<Option<Pin<&'a V>>, E>, E>(
		&self,
		mut local_selector: S,
	) -> Result<Option<(&Node<T, K, V>, Pin<&V>)>, E> {
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

/// # Exclusive Bulk Insertions
///
/// These functions can be batched without incurring locking overhead between them.
///
/// Use [`NodeHandle::as_exclusive`] to acquire a suitable exclusive [`Node`] reference.
impl<T, K: Ord, V: ?Sized> Node<T, K, V> {
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
