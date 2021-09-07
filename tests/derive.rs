#![cfg(feature = "macros")]

use rhizome::{implement_type_keys, sync::TypeKey};
use static_assertions::{assert_impl_all, assert_not_impl_any};
use std::fmt::Debug;
use std::marker::PhantomData;

#[derive(TypeKey)]
enum Enum {}
assert_impl_all!(Enum: TypeKey);

#[derive(TypeKey)]
struct Struct;
assert_impl_all!(Struct: TypeKey);

// A `Self: 'static` bound is added automatically.
#[derive(TypeKey)]
struct GenericStruct<X>(PhantomData<X>);
assert_impl_all!(GenericStruct<()>: TypeKey);

#[derive(TypeKey)]
union Union {
	_x: usize,
}
assert_impl_all!(Union: TypeKey);

trait Trait {}
implement_type_keys!(dyn Trait);
assert_impl_all!(dyn Trait: TypeKey);

// The `impl` coverage can be limited this way too:
struct Test<X>(PhantomData<X>);
implement_type_keys!(Test<X: Debug>);
assert_impl_all!(Test<()>: TypeKey);
assert_not_impl_any!(Test<Test<()>>: TypeKey);

trait Test2<X> {}
implement_type_keys!(dyn Test2<X> where X: Debug);
