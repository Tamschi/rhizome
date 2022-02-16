use super::{Node, NodeHandle};
use fruit_salad::Dyncast;
use std::{
	any::TypeId,
	borrow::{Borrow, BorrowMut},
	convert::Infallible,
	marker::PhantomData,
	ops::Deref,
	option::Option,
	pin::Pin,
	ptr::{addr_of, NonNull},
};
use tap::Pipe;
use this_is_fine::{Fine, FineExt};
use tiptoe::{RefCounter, TipToe};

/// Stored [`Node`] value type used by the traits (dependency injection utilities) in this module.
pub type DynValue = dyn 'static + Send + Sync + Dyncast;

/// Provides the `::inject(node, value)` function.
pub trait Inject<V> {
	/// Tries to inject a `value` into a node
	fn inject<T, C: RefCounter>(
		node: Pin<&Node<T, TypeId, DynValue, C>>,
		value: V,
	) -> Fine<Pin<&DynValue>, V>;
}

/// [GAT](https://rust-lang.github.io/rfcs/1598-generic_associated_types.html) workaround for [`Extract`].
pub trait Extracted<T, C = TipToe> {
	/// The actual type of the extracted value.
	type Extracted;
}

pub struct RefExtractedExtracted<V: ?Sized>(Infallible, PhantomData<V>);
impl<T, V: ?Sized, C: RefCounter> Extracted<T, C> for RefExtractedExtracted<V> {
	type Extracted = RefExtracted<T, V, C>;
}

/// Provides the `::extract(node)` function.
pub trait Extract {
	/// A type constructor determining the type of the extracted resource, by value.
	type Extracted;

	/// Tries to extract this resource from a given `node` or its ancestors.
	///
	/// # Errors
	///
	/// Iff a resource with a matching token was found but could not be cast into the correct type.
	#[allow(clippy::type_complexity)]
	fn extract<T, C: RefCounter>(
		node: Pin<&Node<T, TypeId, DynValue, C>>,
	) -> Result<Option<<Self::Extracted as Extracted<T, C>>::Extracted>, Pin<&DynValue>>
	where
		Self::Extracted: Extracted<T, C>;
}

/// Causes [`Extract`] to be blanket-implemented for `Self`, resolving into a [`RefExtracted`].
pub trait RefExtract {
	/// The type to be extracted behind a shared reference if `Self` is used as token.
	type ExtractedTarget: 'static + ?Sized;
}

/// The actual extracted type is [`RefExtracted`].
impl<V: 'static + ?Sized> Extract for V
where
	V: RefExtract,
{
	type Extracted = RefExtractedExtracted<V::ExtractedTarget>;

	#[allow(clippy::type_complexity)]
	fn extract<T, C: RefCounter>(
		node: Pin<&Node<T, TypeId, DynValue, C>>,
	) -> std::result::Result<
		Option<
			<RefExtractedExtracted<
				<V as RefExtract>::ExtractedTarget,
			> as Extracted<T, C>>::Extracted,
		>,
		Pin<
			&(dyn fruit_salad::Dyncast + std::marker::Send + std::marker::Sync + 'static),
		>,
	>{
		node.get_ref()
			.get(&TypeId::of::<V>())
			.map(|(node, value)| {
				let value_: Pin<&dyn Dyncast> = value;
				RefExtracted {
					handle: node.clone_handle(),
					value: <dyn Dyncast>::dyncast_pinned::<V::ExtractedTarget>(value_)
						.ok_or(value)?
						.pipe(|value| unsafe { Pin::new_unchecked(Dereferenceable::new(&*value)) }),
				}
				.pipe(Ok)
			})
			.transpose()
	}
}

/// An owned handle to a shared dependency.
///
/// Use [`Borrow::borrow`] to get a pinning reference.
pub struct RefExtracted<T, V: ?Sized, C: RefCounter = TipToe> {
	value: Pin<Dereferenceable<V>>,
	handle: NodeHandle<T, TypeId, DynValue, C>,
}
impl<T, V: ?Sized, C: RefCounter> Deref for RefExtracted<T, V, C> {
	type Target = V;

	fn deref(&self) -> &Self::Target {
		&self.value
	}
}
impl<'a, T, V: ?Sized, C: RefCounter> Borrow<Pin<&'a V>> for RefExtracted<T, V, C> {
	fn borrow(&self) -> &Pin<&'a V> {
		unsafe {
			//SAFETY: This casts `*const Dereferenceable<V> -> &Pin<&V>`,
			// which are guaranteed to have the same layout (because both are
			// internally double-pointers to `V` and the structs have `#[repr(transparent)]`).
			&*addr_of!(self.value).cast::<Pin<&V>>()
		}
	}
}

impl<T, V: ?Sized, C: RefCounter> Clone for RefExtracted<T, V, C> {
	fn clone(&self) -> Self {
		Self {
			handle: self.handle.clone(),
			value: Pin::clone(&self.value),
		}
	}
}

/// A pointer guaranteed to be dereferenceable (shared) while it exists.
///
/// > Intentionally not [`Copy`] to make it easier to keep track of.
#[repr(transparent)]
struct Dereferenceable<T: ?Sized>(NonNull<T>);
unsafe impl<T: ?Sized> Send for Dereferenceable<T> {}
unsafe impl<T: ?Sized> Sync for Dereferenceable<T> {}
impl<T: ?Sized> Dereferenceable<T> {
	/// # Safety
	///
	/// The value must stay in place while this instance or one of its clones exist.
	unsafe fn new(ref_: &T) -> Self {
		Self(ref_.into())
	}
}
impl<T: ?Sized> Deref for Dereferenceable<T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		unsafe {
			//SAFETY: This is guaranteed to be valid if `::new`'s soundness contract is followed.
			self.0.as_ref()
		}
	}
}
impl<T: ?Sized> Clone for Dereferenceable<T> {
	fn clone(&self) -> Self {
		Self { ..*self }
	}
}

/// Causes [`Inject`] to be blanket-implemented for `Self`.
///
/// # Example
///
/// ```rust
/// use rhizome::sync::BlanketSizedInject;
///
/// struct Struct;
///
/// impl BlanketSizedInject for Struct {}
///
/// {
///     use rhizome::sync::{Inject, Extract};
///     use static_assertions::{assert_impl_all, assert_not_impl_any};
///
///     assert_impl_all!(Struct: Inject<Struct>);
///     assert_not_impl_any!(Struct: Extract);
/// }
/// ```
pub trait BlanketSizedInject
where
	Self: 'static + Send + Sync + Sized,
{
}

/// Causes [`Inject`] and [`RefExtract`] (and through that [`Extract`]) to be blanket-implemented for `Self`.
///
/// # Example
///
/// ```rust
/// use rhizome::sync::BlanketSizedDependency;
///
/// struct Struct;
///
/// impl BlanketSizedDependency for Struct {}
///
/// {
///     use rhizome::sync::{Inject, RefExtract, Extract};
///     use static_assertions::assert_impl_all;
///
///     assert_impl_all!(Struct: Inject<Struct>, RefExtract, Extract);
/// }
/// ```
pub trait BlanketSizedDependency
where
	Self: 'static + Send + Sync + Sized,
{
}

impl<V> BlanketSizedInject for V where V: BlanketSizedDependency {}

impl<V> Inject<V> for V
where
	V: BlanketSizedInject,
{
	fn inject<T, C: RefCounter>(
		node: Pin<&Node<T, TypeId, DynValue, C>>,
		value: V,
	) -> Fine<Pin<&DynValue>, V> {
		#[derive(Dyncast)]
		#[dyncast(#![runtime_pointer_size_assertion] unsafe V)]
		#[repr(transparent)]
		struct InjectionWrapper<V>(V);
		impl<V: 'static + Send + Sync> Borrow<DynValue> for InjectionWrapper<V> {
			fn borrow(&self) -> &DynValue {
				self
			}
		}
		impl<V: 'static + Send + Sync> BorrowMut<DynValue> for InjectionWrapper<V> {
			fn borrow_mut(&mut self) -> &mut DynValue {
				self
			}
		}

		node.get_ref()
			.emplace(TypeId::of::<V>(), InjectionWrapper(value))
			.map_err(|(_, InjectionWrapper(value))| value)
	}
}

impl<V> RefExtract for V
where
	V: BlanketSizedDependency,
{
	type ExtractedTarget = Self;
}

/// Implements dependency injection for a trait.
///
/// # Example
///
/// ```rust
/// use rhizome::sync::derive_inject;
///
/// trait Trait {}
///
/// derive_inject!(dyn Trait);
///
/// {
///     use rhizome::sync::{Inject, Extract};
///     use static_assertions::{assert_impl_all, assert_not_impl_any};
///
///     struct Struct;
///     impl Trait for Struct {}
///
///     assert_impl_all!(dyn Trait: Inject<Struct>);
///     assert_not_impl_any!(dyn Trait: Extract);
/// }
/// ```
#[macro_export]
macro_rules! derive_inject_sync {
	($(
		$([$($generics:tt)*])? dyn $Trait:path
	),*$(,)?) => {$(
		impl<__rhizome__V> $crate::sync::Inject<__rhizome__V> for dyn $Trait
		where
			__rhizome__V: 'static + Send + Sync + $Trait,
		{
			fn inject<__rhizome__T, __rhizome__C: $crate::__::tiptoe::RefCounter>(
				node: $crate::__::core::pin::Pin<&$crate::sync::Node<__rhizome__T, $crate::__::core::any::TypeId, $crate::sync::DynValue, __rhizome__C>>,
				value: __rhizome__V,
			) -> $crate::__::this_is_fine::Fine<$crate::__::core::pin::Pin<&$crate::sync::DynValue>, __rhizome__V> {
				#[derive($crate::__::fruit_salad::Dyncast)]
				#[fruit_salad($crate::__::fruit_salad)]
				#[dyncast(#![runtime_pointer_size_assertion] unsafe __rhizome__V as (dyn 'static + $crate::__::core::marker::Send + $crate::__::core::marker::Sync + $Trait))]
				#[repr(transparent)]
				struct InjectionWrapper<__rhizome__V: 'static + $crate::__::core::marker::Send + $crate::__::core::marker::Sync + $Trait>(__rhizome__V);
				impl<__rhizome__V: 'static + $crate::__::core::marker::Send + $crate::__::core::marker::Sync + $Trait> $crate::__::core::borrow::Borrow<$crate::sync::DynValue> for InjectionWrapper<__rhizome__V> {
					fn borrow(&self) -> &$crate::sync::DynValue {
						self
					}
				}
				impl<__rhizome__V: 'static + $crate::__::core::marker::Send + $crate::__::core::marker::Sync + $Trait> $crate::__::core::borrow::BorrowMut<$crate::sync::DynValue> for InjectionWrapper<__rhizome__V> {
					fn borrow_mut(&mut self) -> &mut $crate::sync::DynValue {
						self
					}
				}

				$crate::__::this_is_fine::FineExt::map_err(
					node.get_ref().emplace($crate::__::core::any::TypeId::of::<$Trait>(), InjectionWrapper(value)),
					|(_, InjectionWrapper(value))| value,
				)
			}
		}
	)*};
}

pub use crate::derive_inject_sync as derive_inject;

/// Implements dependency injection and extraction for a trait.
///
/// # Example
///
/// ```rust
/// use rhizome::sync::derive_dependency;
///
/// trait Trait {}
///
/// derive_dependency!(dyn Trait);
///
/// {
///     use rhizome::sync::{Inject, RefExtract, Extract};
///     use static_assertions::{assert_impl_all, assert_not_impl_any};
///
///     struct Struct;
///     impl Trait for Struct {}
///
///     assert_impl_all!(dyn Trait: Inject<Struct>, RefExtract, Extract);
/// }
/// ```
#[macro_export]
macro_rules! derive_dependency_sync {
	($(
		$([$($generics:tt)*])? dyn $Trait:path
	),*$(,)?) => {$(
		//TODO: Add a test that ensures these implementations match.
		$crate::sync::derive_inject!(dyn $Trait);

		impl $crate::sync::RefExtract for dyn $Trait {
			type ExtractedTarget = dyn 'static + $crate::__::core::marker::Send + $crate::__::core::marker::Sync + $Trait;
		}
	)*};
}

pub use crate::derive_dependency_sync as derive_dependency;
