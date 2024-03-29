use fruit_salad::Dyncast;
use rhizome::sync::Node;
use std::{
	borrow::{Borrow, BorrowMut},
	fmt::Debug,
};
use this_is_fine::prelude::*;

#[derive(Debug, Dyncast)]
#[dyncast(Self, dyn Debug)]
struct A(&'static str);
impl<'a> Borrow<dyn 'a + Dyncast> for A {
	fn borrow(&self) -> &(dyn 'a + Dyncast) {
		self
	}
}
impl<'a> BorrowMut<dyn 'a + Dyncast> for A {
	fn borrow_mut(&mut self) -> &mut (dyn 'a + Dyncast) {
		self
	}
}

#[test]
fn test() {
	let root = Node::<_, _, dyn Dyncast>::new(1);

	let second = Node::handle_into_branch_for(root, 2);

	let third_a = second.branch_for(3);
	let third_b = Node::handle_into_branch_for(second, 3);

	third_a.root().emplace("at root", A("at root")).unwrap();
	assert!(matches!(
		third_b.get("at root").unwrap().1.dyncast::<A>(),
		Some(A("at root"))
	));

	third_a
		.tagged(&2)
		.unwrap()
		.emplace_with("at 2", |key, slot| slot.write(A(key)))
		.ok()
		.unwrap();
	assert!(matches!(
		third_b.get("at root").unwrap().1.dyncast::<A>(),
		Some(A("at root"))
	));
	assert!(matches!(
		third_b.get("at 2").unwrap().1.dyncast::<A>(),
		Some(A("at 2"))
	));

	third_a.emplace("at third", A("at third (a only)")).unwrap();
	assert!(matches!(third_b.get("at third"), None));
	assert!(matches!(
		third_a.get("at third").unwrap().1.dyncast::<A>(),
		Some(A("at third (a only)"))
	));
}
