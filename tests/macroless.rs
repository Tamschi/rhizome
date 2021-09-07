use core::{
	cell::RefCell,
	fmt::{Display, Error as fmtError, Formatter},
};
use rhizome::{error::Error, sync::extensions::*, sync::Node, sync::TypeKey};
use std::error::Error as stdError;

type V = RefCell<u8>;

#[test]
fn shared_at_root() {
	let root = Node::<V>::new_for_type::<RootOwner>().into_arc();
	let branch_a = root.branch_for_type::<AOwner>();
	let branch_b = root.branch_for_type::<BOwner>();

	*branch_a.extract_with_type_key::<K>().unwrap().borrow_mut() += 1;
	assert_eq!(*branch_b.extract_with_type_key::<K>().unwrap().borrow(), 1);
}

#[test]
fn only_at_a() {
	let root = Node::<V>::new_for_type::<RootOwner>().into_arc();
	let branch_a = root.branch_for_type::<AOwner>();
	let branch_b = root.branch_for_type::<BOwner>();

	assert_eq!(*branch_a.extract_with::<KA, _>().unwrap().borrow(), 0);
	assert_eq!(
		branch_b.extract_with::<KA, _>().unwrap_err(),
		Error::NoTagMatched
	);
}

#[test]
fn not_shared() {
	let root = Node::<V>::new_for::<RootOwner>().into_arc();
	let branch_a = root.branch_for_type::<AOwner>();
	let branch_b = root.branch_for_type::<AOwner>();

	*branch_a.extract_with::<KA, _>().unwrap().borrow_mut() += 1;
	assert_eq!(*branch_b.extract_with::<KA, _>().unwrap().borrow(), 0);
}

#[test]
fn manual_provision() {
	let mut root = Node::<V>::new_for::<RootOwner>();
	assert_eq!(
		root.extract_with::<KManual, _>().unwrap_err(),
		Error::NoDefault
	);

	root.provide(KManual::key(), V::default()).unwrap();
	root.extract_with::<KManual, _>().unwrap();
}

struct RootOwner;
struct AOwner;
struct BOwner;

#[derive(Debug)]
enum Never {}
impl stdError for Never {}
impl Display for Never {
	fn fmt(&self, _: &mut Formatter<'_>) -> Result<(), fmtError> {
		todo!()
	}
}

enum K {}
impl TypeKey<Never, V> for K {
	fn provision() -> Provision<Never, V> {
		Provision::at_root(factory)
	}
}

enum KA {}
impl TypeKey<Never, V> for KA {
	fn provision() -> Provision<Never, V> {
		Provision::at_owner::<AOwner>(factory)
	}
}

enum KManual {}
impl TypeKey<Never, V> for KManual {}

fn factory(_: &Node<V>) -> Result<V, Never> {
	Ok(V::default())
}
