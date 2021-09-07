#![cfg(feature = "macros")]

use rhizome::{implement_type_keys, sync::TypeKey};
use static_assertions::assert_impl_all;

#[derive(TypeKey)]
enum Enum {}
assert_impl_all!(Enum: TypeKey);

#[derive(TypeKey)]
struct Struct;
assert_impl_all!(Struct: TypeKey);

#[derive(TypeKey)]
union Union {
	_x: usize,
}
assert_impl_all!(Union: TypeKey);

trait Trait {}
implement_type_keys!(dyn Trait);
assert_impl_all!(dyn Trait: TypeKey);
