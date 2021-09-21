#![cfg(feature = "macros")]

use rhizome::{implement_dyncasts, implement_type_keys, sync::TypeKey, Dyncast};
use static_assertions::{assert_impl_all, assert_not_impl_any};
use std::{fmt::Debug, marker::PhantomData};

#[derive(Dyncast, TypeKey)]
enum Enum {}
assert_impl_all!(Enum: Dyncast, TypeKey);

#[derive(Dyncast, TypeKey)]
struct Struct;
assert_impl_all!(Struct: Dyncast, TypeKey);

// A `Self: 'static` bound is added automatically.
#[derive(Dyncast, TypeKey)]
struct GenericStruct<X>(PhantomData<X>);
assert_impl_all!(GenericStruct<()>: Dyncast, TypeKey);

#[derive(Dyncast, TypeKey)]
union Union {
	_x: usize,
}
assert_impl_all!(Union: Dyncast, TypeKey);

trait Trait {}
implement_dyncasts!(dyn Trait);
implement_type_keys!(dyn Trait);
assert_impl_all!(dyn Trait: Dyncast, TypeKey);

// The `impl` coverage can be limited this way too:
struct Test<X>(PhantomData<X>);
implement_dyncasts!(Test<X: Debug>);
implement_type_keys!(Test<X: Debug>);
assert_impl_all!(Test<()>: Dyncast, TypeKey);
assert_not_impl_any!(Test<Test<()>>: Dyncast, TypeKey);

trait Test2<X> {}
implement_dyncasts!(dyn Test2<X> where X: Debug);
implement_type_keys!(dyn Test2<X> where X: Debug);
assert_impl_all!(dyn Test2<()>: Dyncast, TypeKey);
assert_not_impl_any!(dyn Test2<Test<()>>: Dyncast, TypeKey);
