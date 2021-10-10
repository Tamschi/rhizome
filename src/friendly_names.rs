use core::{
	any::{Any, TypeId},
	borrow::Borrow,
	hash::Hash,
};
use static_init::dynamic;
use std::collections::{hash_map::Entry, HashMap};

#[dynamic]
static mut FRIENDLY_NAMES: HashMap<TypeId, Box<dyn Send + Sync + Any>> = HashMap::new();

/// Manages friendly names for [`Error`](`crate::error::Error`) traces.
pub struct FriendlyNames<T>(HashMap<T, String>);
impl<T> FriendlyNames<T> {
	fn new() -> Self {
		Self(HashMap::new())
	}

	/// Registers a new friendly name for a given `key`.
	///
	/// # Errors
	///
	/// Iff the key already had an associated friendly name, in which case nothing is done.
	pub fn register<F: FnOnce() -> String>(key: T, friendly: F) -> Result<(), F>
	where
		T: 'static + Send + Sync + Eq + Hash,
	{
		match FRIENDLY_NAMES
			.write()
			.entry(TypeId::of::<Self>())
			.or_insert_with(|| Box::new(Self::new()))
			.downcast_mut::<Self>()
			.expect("unreachable")
			.0
			.entry(key)
		{
			Entry::Occupied(_) => Err(friendly),
			Entry::Vacant(vacant) => {
				vacant.insert(friendly());
				Ok(())
			}
		}
	}

	/// Retrieves a friendly name for `key`, if available.
	pub fn get<Q>(key: &Q) -> Option<String>
	where
		T: 'static + Eq + Hash + Borrow<Q>,
		Q: Hash + Eq,
	{
		FRIENDLY_NAMES
			.read()
			.get(&TypeId::of::<Self>())
			.and_then(|this| {
				this.downcast_ref::<Self>()
					.expect("unreachable")
					.0
					.get(key)
					.cloned()
			})
	}
}
