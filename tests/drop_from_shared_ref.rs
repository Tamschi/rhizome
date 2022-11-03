use rhizome::sync::Node;

#[test]
fn drop_from_shared_ref() {
	let a = Node::<_, Box<usize>, Box<usize>>::new(Box::new(0));
	let b = a.as_ref().clone_handle();
	drop(a);
	drop(b);
}
