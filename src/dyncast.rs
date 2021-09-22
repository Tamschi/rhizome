use std::{
	any::TypeId,
	cmp::Ordering,
	fmt::{self, Debug, Display},
	hash::{Hash, Hasher},
	mem::{self, MaybeUninit},
	ops::Deref,
	pin::Pin,
	ptr::NonNull,
};

#[cfg(feature = "macros")]
pub use rhizome_proc_macro_definitions::{implement_dyncasts, Dyncast};

impl dyn Dyncast {
	#[allow(missing_docs)]
	#[must_use]
	pub fn dyncast<T: 'static + ?Sized>(&self) -> Option<&T> {
		self.__dyncast(
			unsafe { NonNull::new_unchecked(self as *const Self as *mut Self) }.cast(),
			TypeId::of::<T>(),
		)
		.map(|pointer_data| unsafe { pointer_data.as_ptr().cast::<&T>().read_unaligned() })
	}

	#[allow(missing_docs)]
	#[must_use]
	pub fn dyncast_mut<T: 'static + ?Sized>(&mut self) -> Option<&mut T> {
		let this = unsafe { NonNull::new_unchecked(self) };
		self.__dyncast(this.cast(), TypeId::of::<T>())
			.map(|pointer_data| unsafe { pointer_data.as_ptr().cast::<&mut T>().read_unaligned() })
	}

	#[allow(missing_docs)]
	#[must_use]
	pub fn dyncast_pinned<'a, T: 'static + ?Sized>(self: Pin<&'a Self>) -> Option<Pin<&'a T>> {
		self.__dyncast(
			unsafe { NonNull::new_unchecked(&*self as *const Self as *mut Self) }.cast(),
			TypeId::of::<T>(),
		)
		.map(|pointer_data| unsafe { pointer_data.as_ptr().cast::<Pin<&T>>().read_unaligned() })
	}

	#[allow(missing_docs)]
	#[must_use]
	pub fn dyncast_pinned_mut<'a, T: 'static + ?Sized>(
		mut self: Pin<&'a mut Self>,
	) -> Option<Pin<&'a mut T>> {
		let this = unsafe {
			NonNull::new_unchecked(Pin::into_inner_unchecked(self.as_mut()) as *mut Self)
		};
		self.__dyncast(this.cast(), TypeId::of::<T>())
			.map(|pointer_data| unsafe {
				pointer_data.as_ptr().cast::<Pin<&mut T>>().read_unaligned()
			})
	}
}

/// Reference downcasting, also to pinned trait objects.
///
/// # ☡ Potential Trip-ups
///
/// ## Dynamic formatting
///
/// Values will be formatted as `dyn Dyncast = !dyn Debug` or `dyn Dyncast = !dyn Display`
/// if they are not *dynamically* [`Debug`] or not *dynamically* [`Display`], respectively.
///
/// Add `#[dyncast(dyn Debug)]` and/or `#[dyncast(dyn Display)]` where you derive or implement this trait.
///
/// ## Partial comparisons
///
/// [`dyn Dyncast`](`Dyncast`) trait object comparisons through [`PartialEq`] and [`PartialOrd`] will always return [`false`] or [`None`] (respectively)
/// unless at least one of the underlying types is *dynamically* [`PartialEq<dyn Dyncast>`] or *dynamically* [`PartialOrd<dyn Dyncast>`], respectively.
///
/// > These implementations should mirror each other if both are available.
///
/// You can write `#[dyncast(impl dyn PartialEq<dyn Dyncast>)]` and `#[dyncast(impl dyn PartialOrd<dyn Dyncast>)]` to
/// implement these automatically, respectively based on the plain [`PartialEq`] and [`PartialOrd`] implementations.
///
/// Comparisons between distinct types will always result in [`false`] or [`None`] with the automatic implementations.
///
/// ## Complete comparisons
///
/// [`Dyncast`] alone never exposes complete comparisons without explicit dyncast.
///
/// However, the following subtraits are available for more completely comparable values:
/// [`DyncastEq`], [`DyncastOrd`], and [`DyncastEqOrd`] which is both [`DyncastEq`] and [`DyncastOrd`].
///
/// [`DyncastEq`] and [`DyncastOrd`] can be implemented manually.
/// [`DyncastEqOrd`] is available only through its blanket implementation on types that are both of the former.
///
/// [`DyncastEq`] is implemented automatically iff you write `#[dyncast(impl dyn PartialEq<dyn Dyncast>)]`,
/// conditional on `Self` being [`Eq`].
///
/// [`DyncastOrd`] is implemented automatically iff you write `#[dyncast(impl dyn PartialOrd<dyn Dyncast>, Self)]`,
/// conditional on `Self` being [`Ord`] and [`DyncastEq`].
///
/// ## Hashing
///
/// Meaningful hashing requires that the underlying type be *dynamically* [`DynHash`].
///
/// A blanked implementation is available for types that are [`Hash`],
/// but you still need to enable dyncasts using `#[dyncast(dyn DynHash)]`.
///
/// Types that are not dynamically [`DynHash`] hash dynamically by not hashing anything.
///
/// For convenience, you can enable dyncasts without importing [`DynHash`] by writing `#[dyncast(impl dyn DynHash)]`.
///
/// # ☡ Cognitohazard Warning ☡
///
/// There is Generally Not Neat code here.
///
/// Use the [`Dyncast`](./derive.Dyncast.html) derive macro to implement this and don't worry about it too much.
///
/// I've put in some const assertions and don't rely on unstable behaviour,
/// so this shouldn't fail silently, at least.
///
/// > **Wishlist**
/// >
/// > - `self: NonNull<Self>` as receiver on object-safe `unsafe` trait methods.
/// > - [#81513](https://github.com/rust-lang/rust/issues/81513) or similar.
pub unsafe trait Dyncast {
	#[allow(clippy::type_complexity)]
	#[doc(hidden)]
	fn __dyncast(
		&self,
		this: NonNull<()>,
		target: TypeId,
	) -> Option<MaybeUninit<[u8; mem::size_of::<&dyn Dyncast>()]>>;
}

impl Debug for dyn Dyncast {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str("dyn Dyncast = ")?;
		#[allow(clippy::option_if_let_else)] // Can't because `f`.
		if let Some(debug) = self.dyncast::<dyn Debug>() {
			debug.fmt(f)
		} else {
			f.write_str("!dyn Debug")
		}
	}
}

impl Display for dyn Dyncast {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		#[allow(clippy::option_if_let_else)] // Can't because `f`.
		if let Some(display) = self.dyncast::<dyn Display>() {
			display.fmt(f)
		} else {
			f.write_str("dyn Dyncast = !dyn Display")
		}
	}
}

impl PartialEq for dyn Dyncast {
	fn eq(&self, other: &(dyn 'static + Dyncast)) -> bool {
		if let Some(this) = self.dyncast::<dyn PartialEq<dyn Dyncast>>() {
			this.eq(other)
		} else if let Some(other) = other.dyncast::<dyn PartialEq<dyn Dyncast>>() {
			other.eq(self)
		} else {
			false
		}
	}
}

impl PartialOrd for dyn Dyncast {
	fn partial_cmp(&self, other: &(dyn 'static + Dyncast)) -> Option<Ordering> {
		if let Some(this) = self.dyncast::<dyn PartialOrd<dyn Dyncast>>() {
			this.partial_cmp(other)
		} else if let Some(other) = other.dyncast::<dyn PartialOrd<dyn Dyncast>>() {
			other.partial_cmp(self).map(Ordering::reverse)
		} else {
			None
		}
	}
}

/// Object-safe [`Hash`].
pub trait DynHash {
	fn hash(&self, state: &mut dyn Hasher);
}
impl<T: ?Sized> DynHash for T
where
	T: Hash,
{
	fn hash(&self, mut state: &mut dyn Hasher) {
		<Self as Hash>::hash(self, &mut state)
	}
}

impl Hash for dyn Dyncast {
	fn hash<H: Hasher>(&self, state: &mut H) {
		if let Some(dyn_hash) = self.dyncast::<dyn DynHash>() {
			dyn_hash.hash(state)
		}
	}
}

/// [`Dyncast`] and *dynamically* [`Eq`]
//TODO: Other traits
pub trait DyncastEq: Dyncast {}
impl Deref for dyn DyncastEq {
	type Target = dyn Dyncast;

	fn deref(&self) -> &Self::Target {
		self
	}
}
impl PartialEq for dyn DyncastEq {
	fn eq(&self, other: &Self) -> bool {
		self.dyncast::<dyn PartialEq<dyn Dyncast>>()
			.expect("Expected `Self` to be *dynamically* `dyn PartialEq<dyn Dyncast>` via `dyn DyncastEq: PartialEq`")
			.eq(other)
	}
}
impl Eq for dyn DyncastEq {}

/// [`DyncastEq`] and *dynamically* [`Ord`]
//TODO: Other traits
pub trait DyncastOrd: DyncastEq {
	fn cmp(&self, other: &dyn DyncastOrd) -> Ordering;
}
impl Deref for dyn DyncastOrd {
	type Target = dyn DyncastEq;

	fn deref(&self) -> &Self::Target {
		self
	}
}
impl PartialEq for dyn DyncastOrd {
	fn eq(&self, other: &Self) -> bool {
		let this: &dyn DyncastEq = self;
		this.eq(other)
	}
}
impl Eq for dyn DyncastOrd {}
impl PartialOrd for dyn DyncastOrd {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		self.dyncast::<dyn PartialOrd<dyn Dyncast>>()
		.expect("Expected `Self` to be *dynamically* `dyn PartialOrd<dyn Dyncast>` via `dyn DyncastOrd: PartialOrd`")
		.partial_cmp(other)
	}
}
impl Ord for dyn DyncastOrd {
	fn cmp(&self, other: &Self) -> Ordering {
		Self::cmp(self, other)
	}
}
