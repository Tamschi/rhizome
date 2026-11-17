use std::{any::TypeId, pin::Pin, sync::Arc};

use rhizome::{Injectable, Tag, Waypoint};

trait ATag {}
impl Tag for dyn ATag {}

trait AnInjectable: Send + Sync {}
impl Injectable for dyn AnInjectable {
    const INJECT_AT: &'static [TypeId] = &[];

    type ErrorSet = ();

    fn provision(_tag: Option<TypeId>) -> eros::Result<Pin<Arc<Self>>, Self::ErrorSet> {
        struct Struct;
        impl AnInjectable for Struct {}
        Ok(Arc::pin(Struct))
    }

    type Provided = Pin<Arc<Self>>;

    fn provide(self: Pin<Arc<Self>>) -> Self::Provided {
        self
    }
}

#[test]
fn demo() {
    let mut waypoint = Waypoint::new_root::<dyn ATag>();
    waypoint
        .provide(<dyn AnInjectable>::provision(TypeId::of::<dyn ATag>().into()).expect(""))
        .ok()
        .unwrap();
    waypoint
        .provide(<dyn AnInjectable>::provision(TypeId::of::<dyn ATag>().into()).expect(""))
        .err()
        .unwrap();

    let mut waypoint = waypoint.new::<dyn ATag>();
    waypoint
        .provide(<dyn AnInjectable>::provision(TypeId::of::<dyn ATag>().into()).expect(""))
        .ok()
        .unwrap();
    waypoint
        .provide(<dyn AnInjectable>::provision(TypeId::of::<dyn ATag>().into()).expect(""))
        .err()
        .unwrap();

    let mut waypoint2 = waypoint.lift_and_new::<dyn ATag>();
    waypoint2
        .provide(<dyn AnInjectable>::provision(TypeId::of::<dyn ATag>().into()).expect(""))
        .ok()
        .unwrap();
    drop(waypoint);

    let mut waypoint = waypoint2.new::<dyn ATag>();
    waypoint
        .provide(<dyn AnInjectable>::provision(TypeId::of::<dyn ATag>().into()).expect(""))
        .ok()
        .unwrap();
    waypoint
        .provide(<dyn AnInjectable>::provision(TypeId::of::<dyn ATag>().into()).expect(""))
        .err()
        .unwrap();

    waypoint.retract_local::<dyn AnInjectable>().unwrap();
    waypoint
        .retract_local::<dyn AnInjectable>()
        .ok_or(())
        .err()
        .unwrap();

    let b = waypoint.extract::<dyn AnInjectable>().unwrap();
    let a = waypoint2.extract::<dyn AnInjectable>().unwrap();
    assert_eq!(&raw const *a, &raw const *b);

    waypoint2.retract_local::<dyn AnInjectable>().unwrap();
    waypoint2
        .provide(<dyn AnInjectable>::provision(TypeId::of::<dyn ATag>().into()).expect(""))
        .ok()
        .unwrap();
    waypoint2.retract_local::<dyn AnInjectable>().unwrap();

    Waypoint::new_root::<dyn ATag>()
        .extract::<dyn AnInjectable>()
        .unwrap();
}
