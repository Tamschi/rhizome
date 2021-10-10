#![cfg(FALSE)]

use core::cell::RefCell;
use rhizome::sync::NodeHandle;
use std::{any::TypeId, convert::Infallible};
use tap::Pipe;

type V = RefCell<u8>;

#[test]
fn shared_at_root() {
	let root = NodeHandle::<_, _, V>::new_with_type_tag::<RootOwner>().into_arc();
	let branch_a = root.branch_with_type_tag::<AOwner>();
	let branch_b = root.branch_with_type_tag::<BOwner>();

	*branch_a.extract_by_type_key::<K>().unwrap().borrow_mut() += 1;
	assert_eq!(*branch_b.extract_by_type_key::<K>().unwrap().borrow(), 1);
}

#[test]
fn only_at_a() {
	let root = NodeHandle::<_, _, V>::new_with_type_tag::<RootOwner>().into_arc();
	let branch_a = root.branch_with_type_tag::<AOwner>();
	let branch_b = root.branch_with_type_tag::<BOwner>();

	assert_eq!(*branch_a.extract_by_type_key::<KA>().unwrap().borrow(), 0);
	assert!(branch_b.extract_by_type_key::<KA>().is_err());
}

#[test]
fn not_shared() {
	let root = NodeHandle::<_, _, V>::new_with_type_tag::<RootOwner>().into_arc();
	let branch_a = root.branch_with_type_tag::<AOwner>();
	let branch_b = root.branch_with_type_tag::<AOwner>();

	*branch_a.extract_by_type_key::<KA>().unwrap().borrow_mut() += 1;
	assert_eq!(*branch_b.extract_by_type_key::<KA>().unwrap().borrow(), 0);
}

#[test]
fn manual_provision() {
	let root = NodeHandle::<_, _, V>::new_with_type_tag::<RootOwner>();
	root.extract_by_type_key::<KManual>().unwrap_err();

	root.provide(KManual::key(), V::default()).unwrap();
	root.extract_by_type_key::<KManual>().unwrap();
}

struct RootOwner;
struct AOwner;
struct BOwner;

enum K {}
impl TypeKey<V> for K {
	type Error = Infallible;

	fn auto_provide(queried_node: &Node<V>) -> Result<Option<&V>, Self::Error> {
		queried_node
			.root()
			.provide(TypeId::of::<Self>(), 1.into())
			.expect("unreachable")
			.pipe(Some)
			.pipe(Ok)
	}
}

enum KA {}
impl TypeKey<V> for KA {
	type Error = Infallible;
}

enum KManual {}
impl TypeKey<V> for KManual {
	type Error = Infallible;
}
