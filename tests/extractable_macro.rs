#![cfg(FALSE)]
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
	let mut branch = root.branch_for_type::<Owner>();

	assert_eq!(
		<dyn Extractable>::extract_from(&branch).unwrap().type_id(),
		TypeId::of::<ExtractableImpl>()
	);
	assert_eq!(
		<dyn Extractable>::extract_from(&root).unwrap().type_id(),
		TypeId::of::<ExtractableImpl>()
	);

	<dyn Extractable>::provide_custom(&mut branch, Fake {}).unwrap();
	assert_eq!(
		<dyn Extractable>::extract_from(&branch).unwrap().type_id(),
		TypeId::of::<Fake>()
	);
	assert_eq!(
		<dyn Extractable>::extract_from(&root).unwrap().type_id(),
		TypeId::of::<ExtractableImpl>()
	);
}
