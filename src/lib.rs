//! Convenient hierarchical dependency-extraction containers, implemented as reverse trees.
//!
//! This is a data structure that can be used to implement an inversion of control pattern and a Rust-compatible equivalent of dependency-injection,
//! to enable lazy resource provision, resource shadowing and testing/configuration use cases.

#![doc(html_root_url = "https://docs.rs/rhizome/0.0.1")]
#![warn(clippy::pedantic, missing_docs)]
#![allow(clippy::semicolon_if_nothing_returned)]

#[cfg(doctest)]
pub mod readme {
	doc_comment::doctest!("../README.md");
}

pub mod error;
pub mod friendly_names;
pub mod sync;

use std::{
	any::TypeId,
	convert::Infallible,
	mem::{self, MaybeUninit},
	pin::Pin,
	ptr::NonNull,
};

#[cfg(feature = "macros")]
pub use rhizome_proc_macro_definitions::{
	extractable, implement_dyncasts, implement_type_keys, Dyncast, TypeKey,
};
use tap::Pipe;

trait UnwrapInfallible {
	type T;
	fn unwrap_infallible(self) -> Self::T;
}
impl<T> UnwrapInfallible for Result<T, Infallible> {
	type T = T;
	fn unwrap_infallible(self) -> Self::T {
		self.unwrap()
	}
}

/// Reference downcasting, also to pinned trait objects.
pub trait Dyncast: DyncastObject {
	#[allow(missing_docs)]
	#[must_use]
	fn dyncast<T: 'static + ?Sized>(&self) -> Option<&T> {
		let this = self as *const _ as *mut _;
		self.__dyncast()(unsafe { NonNull::new_unchecked(this) }, TypeId::of::<T>())
			.map(|pointer_data| unsafe { pointer_data.as_ptr().cast::<&T>().read_unaligned() })
	}

	#[allow(missing_docs)]
	#[must_use]
	fn dyncast_mut<T: 'static + ?Sized>(&mut self) -> Option<&mut T> {
		#[allow(clippy::ptr_as_ptr)]
		let this = self as *mut _ as *mut _;
		self.__dyncast()(unsafe { NonNull::new_unchecked(this) }, TypeId::of::<T>())
			.map(|pointer_data| unsafe { pointer_data.as_ptr().cast::<&mut T>().read_unaligned() })
	}

	#[allow(missing_docs)]
	#[must_use]
	fn dyncast_pinned<'a, T: 'static + ?Sized>(self: Pin<&'a Self>) -> Option<Pin<&'a T>> {
		let this = &*self as *const _ as *mut _;
		self.__dyncast()(unsafe { NonNull::new_unchecked(this) }, TypeId::of::<T>())
			.map(|pointer_data| unsafe { pointer_data.as_ptr().cast::<Pin<&T>>().read_unaligned() })
	}

	#[allow(missing_docs)]
	#[must_use]
	fn dyncast_pinned_mut<'a, T: 'static + ?Sized>(
		mut self: Pin<&'a mut Self>,
	) -> Option<Pin<&'a mut T>> {
		let this = unsafe { Pin::into_inner_unchecked(self.as_mut()) } as *const _ as *mut _;
		self.__dyncast()(unsafe { NonNull::new_unchecked(this) }, TypeId::of::<T>()).map(
			|pointer_data| unsafe { pointer_data.as_ptr().cast::<Pin<&mut T>>().read_unaligned() },
		)
	}
}
impl<T: ?Sized> Dyncast for T where T: DyncastObject {}

/// Object-safe backing trait for [`Dyncast`].
///
/// Some horribleness.
///
/// Use the [`Dyncast`] derive macro to implement this and don't worry about it too much.
pub unsafe trait DyncastObject {
	#[allow(clippy::type_complexity)]
	#[doc(hidden)]
	fn __dyncast(
		&self,
	) -> fn(
		this: NonNull<()>,
		target: TypeId,
	) -> Option<MaybeUninit<[u8; mem::size_of::<&dyn DyncastObject>()]>>;
}

#[doc(hidden)]
pub mod __ {
	use std::{any::TypeId, mem::MaybeUninit, ptr::NonNull};

	#[cfg(feature = "macros")]
	pub use static_assertions::const_assert;
}
