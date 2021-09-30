use fruit_salad::Dyncast;
use rhizome::sync::{extensions::*, Node};
use std::{
	borrow::{Borrow, BorrowMut},
	fmt::Debug,
};

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
	let root = Node::<_, &str, dyn Dyncast>::new(1).into_arc();

	let second = root.into_branch_for(2).into_arc();

	let third_a = second.branch_for(3);
	let third_b = second.into_branch_for(3);

	third_a.root().emplace("at root", A("at root")).unwrap();
	assert!(matches!(
		third_b.get("at root").unwrap().1.dyncast::<A>().unwrap(),
		A("at root")
	));
}
