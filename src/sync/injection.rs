use super::{Node, NodeHandle};
use fruit_salad::Dyncast;
use std::{
	any::TypeId,
	borrow::{Borrow, BorrowMut},
	mem,
	ops::Deref,
	pin::Pin,
};
use tap::Pipe;
use this_is_fine::{Fine, FineExt};
use tiptoe::RefCounter;

/// Used by this module as value type inside each [`Node`].
pub type DynValue = dyn 'static + Send + Sync + Dyncast;

/// Provides the `::inject(node, value)` function.
pub trait Injectable<V> {
	/// Tries to inject a `value` into a node
	fn inject<T, C: RefCounter>(
		node: Pin<&Node<T, TypeId, DynValue, C>>,
		value: V,
	) -> Fine<Pin<&DynValue>, V>;
}

/// Provides the `::extract(node)` function.
pub trait Extractable<T, C: RefCounter> {
	/// The type of the extracted resource, by value.
	type Extracted;

	/// Tries to extract this resource from a given `node` or its ancestors.
	///
	/// # Errors
	///
	/// Iff a resource with a matching token was found but could not be cast into the correct type.
	fn extract(
		node: Pin<&Node<T, TypeId, DynValue, C>>,
	) -> Result<Option<Self::Extracted>, Pin<&DynValue>>;
}

/// Causes [`Extractable`] to be blanket-implemented for `Self`, resolving into a [`RefExtracted`].
pub trait RefExtractable {
	/// The type to be extracted behind a shared reference if `Self` is used as token.
	type ExtractedTarget: 'static + ?Sized;
}
impl<T, V: 'static + ?Sized, C: RefCounter> Extractable<T, C> for V
where
	V: RefExtractable,
{
	type Extracted = RefExtracted<T, V::ExtractedTarget, C>;

	fn extract(
		node: Pin<&Node<T, TypeId, DynValue, C>>,
	) -> Result<Option<Self::Extracted>, Pin<&DynValue>> {
		node.get_ref()
			.get(&TypeId::of::<V>())
			.map(|(node, value)| {
				let value_: Pin<&dyn Dyncast> = value;
				RefExtracted {
					_handle: node.clone_handle(),
					value: <dyn Dyncast>::dyncast_pinned::<V::ExtractedTarget>(value_)
						.ok_or(value)?
						.pipe(|value| unsafe {
							mem::transmute::<Pin<&V::ExtractedTarget>, Pin<*const V::ExtractedTarget>>(
								value,
							)
						}),
				}
				.pipe(Ok)
			})
			.transpose()
	}
}

/// An owned handle to a shared dependency.
///
/// Use `.borrow()` to get a pinning reference.
pub struct RefExtracted<T, V: ?Sized, C: RefCounter> {
	_handle: NodeHandle<T, TypeId, DynValue, C>,
	value: Pin<*const V>,
}
impl<T, V: ?Sized, C: RefCounter> Deref for RefExtracted<T, V, C> {
	type Target = V;

	fn deref(&self) -> &Self::Target {
		unsafe { mem::transmute::<Pin<*const V>, &V>(self.value) }
	}
}
impl<'a, T, V: ?Sized, C: RefCounter> Borrow<Pin<&'a V>> for RefExtracted<T, V, C> {
	fn borrow(&self) -> &Pin<&'a V> {
		unsafe { &*(&self.value as *const std::pin::Pin<*const V>).cast::<std::pin::Pin<&V>>() }
	}
}

/// Causes [`Injectable`] to be blanket-implemented for `Self`.
///
/// # Example
///
/// ```rust
/// use rhizome::sync::{BlanketSizedInjectable, Injectable, Extractable};
///
/// struct Struct;
/// ```
pub trait BlanketSizedInjectable
where
	Self: 'static + Send + Sync + Sized,
{
}

/// Causes [`Injectable`] and [`RefExtractable`] (and through that [`Extractable`]) to be blanket-implemented for `Self`.
pub trait BlanketSizedDependency
where
	Self: 'static + Send + Sync + Sized,
{
}

impl<V> BlanketSizedInjectable for V where V: BlanketSizedDependency {}

impl<V> Injectable<V> for V
where
	V: BlanketSizedInjectable,
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

impl<V> RefExtractable for V
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
/// use rhizome::sync::derive_trait_injectable;
///
/// trait Trait {}
///
/// derive_trait_injectable!(dyn Trait);
/// ```
#[macro_export]
macro_rules! derive_trait_injectable_sync {
	($(
		$([$($generics:tt)*])? dyn $Trait:path
	),*$(,)?) => {$(
		impl<__rhizome__V> $crate::sync::Injectable<__rhizome__V> for dyn $Trait
		where
			__rhizome__V: 'static + Send + Sync + $Trait,
		{
			fn inject<__rhizome__T, __rhizome__C: $crate::__::tiptoe::RefCounter>(
				node: ::core::pin::Pin<&$crate::sync::Node<__rhizome__T, ::core::any::TypeId, $crate::sync::DynValue, __rhizome__C>>,
				value: __rhizome__V,
			) -> $crate::__::this_is_fine::Fine<::core::pin::Pin<&$crate::sync::DynValue>, __rhizome__V> {
				#[derive($crate::__::fruit_salad::Dyncast)]
				#[dyncast(#![runtime_pointer_size_assertion] unsafe __rhizome__V as dyn $Trait)]
				#[repr(transparent)]
				struct InjectionWrapper<__rhizome__V: 'static + ::core::marker::Send + ::core::marker::Sync + $Trait>(__rhizome__V);
				impl<__rhizome__V: 'static + ::core::marker::Send + ::core::marker::Sync + $Trait> ::core::borrow::Borrow<$crate::sync::DynValue> for InjectionWrapper<__rhizome__V> {
					fn borrow(&self) -> &$crate::sync::DynValue {
						self
					}
				}
				impl<__rhizome__V: 'static + ::core::marker::Send + ::core::marker::Sync + $Trait> ::core::borrow::BorrowMut<$crate::sync::DynValue> for InjectionWrapper<__rhizome__V> {
					fn borrow_mut(&mut self) -> &mut $crate::sync::DynValue {
						self
					}
				}

				$crate::__::this_is_fine::FineExt::map_err(
					node.get_ref().emplace(::core::any::TypeId::of::<$Trait>(), InjectionWrapper(value)),
					|(_, InjectionWrapper(value))| value,
				)
			}
		}
	)*};
}

pub use crate::derive_trait_injectable_sync as derive_trait_injectable;

/// Implements dependency injection and extraction for a trait.
///
/// # Example
///
/// ```rust
/// use rhizome::sync::derive_trait_dependency;
///
/// trait Trait {}
///
/// derive_trait_dependency!(dyn Trait);
/// ```
#[macro_export]
macro_rules! derive_trait_dependency_sync {
	($(
		$([$($generics:tt)*])? dyn $Trait:path
	),*$(,)?) => {$(
		$crate::sync::derive_trait_injectable!(dyn $Trait);

		impl $crate::sync::RefExtractable for dyn $Trait {
			type ExtractedTarget = dyn 'static + ::core::marker::Send + ::core::marker::Sync + $Trait;
		}
	)*};
}

pub use crate::derive_trait_dependency_sync as derive_trait_dependency;
