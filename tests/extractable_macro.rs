#![cfg(feature = "macros")]

use rhizome::extractable;
use std::any::TypeId;

extractable! {
	/// Extractable attributes.
	pub trait Extractable: Debug for final
}

#[test]
fn test_extractable() {
	use rhizome::{extensions::*, Node};

	enum Owner {}

	#[derive(Debug)]
	struct Fake;
	impl Extractable for Fake {}

	let root = Node::new_for::<Owner>().into_arc();
	let mut branch = root.derive_for::<Owner>();

	assert_eq!(
		Extractable::extract_from(&branch).unwrap().type_id(),
		TypeId::of::<ExtractableImpl>()
	);
	assert_eq!(
		Extractable::extract_from(&root).unwrap().type_id(),
		TypeId::of::<ExtractableImpl>()
	);

	Extractable::provide_custom(&mut branch, Fake {}).unwrap();
	assert_eq!(
		Extractable::extract_from(&branch).unwrap().type_id(),
		TypeId::of::<Fake>()
	);
	assert_eq!(
		Extractable::extract_from(&root).unwrap().type_id(),
		TypeId::of::<ExtractableImpl>()
	);
}
