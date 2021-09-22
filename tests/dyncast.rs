#![cfg(feature = "macros")]

use rhizome::Dyncast;
use std::fmt::Debug;

#[derive(Debug, Dyncast)]
#[dyncast(dyn Debug, Self)]
#[dyncast(dyn Debug, Self)]
struct Test;

#[derive(Debug, Dyncast)]
#[dyncast(dyn Debug, unsafe Test)]
struct Test2(String);

#[test]
fn dyncast() {
	let dyncasts: [Box<dyn Dyncast>; 2] = [Box::new(Test), Box::new(Test2("Hello!".to_string()))];

	for dyncast in &dyncasts {
		let debug = dyncast.dyncast::<dyn Debug>();
		assert!(debug.is_some());
		println!("{:#?}", debug);
	}
}
