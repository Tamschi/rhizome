#![cfg(feature = "macros")]

use rhizome::{Dyncast, DyncastObject};
use std::fmt::Debug;

#[derive(Debug, Dyncast)]
#[dyncast(dyn Debug, Self)]
struct Test;

#[derive(Debug, Dyncast)]
#[dyncast(dyn Debug)]
struct Test2(String);

#[test]
fn dyncast() {
	let dyncasts: [Box<dyn DyncastObject>; 2] =
		[Box::new(Test), Box::new(Test2("Hello!".to_string()))];

	for dyncast in &dyncasts {
		let debug = dyncast.dyncast::<dyn Debug>();
		assert!(debug.is_some());
		eprintln!("{:#?}", debug);
	}
}
