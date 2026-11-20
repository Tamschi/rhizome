use std::{any::TypeId, pin::Pin, sync::Arc};

use rhizome::{Injectable, Tag, Waypoint};

trait ATag {}
impl Tag for dyn ATag {}

trait AnInjectable: Send + Sync {}
impl Injectable for dyn AnInjectable {
    const PROVISION_AT: &'static [TypeId] = &[];

    type Error = !;

    fn provision(
        _at: &mut Waypoint<'_>,
        _matched_tag: Option<TypeId>,
    ) -> Result<Pin<Arc<Self>>, Self::Error> {
        struct Struct;
        impl AnInjectable for Struct {}
        Ok(Arc::pin(Struct))
    }

    type Provided = Pin<Arc<Self>>;

    fn provide(self: &Pin<Arc<Self>>) -> Self::Provided {
        Pin::clone(self)
    }
}

#[test]
fn demo() {
    let mut waypoint = Waypoint::new_root::<dyn ATag>();
    let value =
        <dyn AnInjectable>::provision(&mut waypoint, TypeId::of::<dyn ATag>().into()).expect("");
    waypoint.provide(value).ok().unwrap();
    let value1 =
        <dyn AnInjectable>::provision(&mut waypoint, TypeId::of::<dyn ATag>().into()).expect("");
    waypoint.provide(value1).err().unwrap();

    let mut waypoint = waypoint.new::<dyn ATag>();
    let value2 =
        <dyn AnInjectable>::provision(&mut waypoint, TypeId::of::<dyn ATag>().into()).expect("");
    waypoint.provide(value2).ok().unwrap();
    let value3 =
        <dyn AnInjectable>::provision(&mut waypoint, TypeId::of::<dyn ATag>().into()).expect("");
    waypoint.provide(value3).err().unwrap();

    let mut waypoint2 = waypoint.lift_and_new::<dyn ATag>();
    let value4 =
        <dyn AnInjectable>::provision(&mut waypoint2, TypeId::of::<dyn ATag>().into()).expect("");
    waypoint2.provide(value4).ok().unwrap();
    drop(waypoint);

    let mut waypoint = waypoint2.new::<dyn ATag>();
    let value5 =
        <dyn AnInjectable>::provision(&mut waypoint, TypeId::of::<dyn ATag>().into()).expect("");
    waypoint.provide(value5).ok().unwrap();
    let value6 =
        <dyn AnInjectable>::provision(&mut waypoint, TypeId::of::<dyn ATag>().into()).expect("");
    waypoint.provide(value6).err().unwrap();

    let b = waypoint.extract::<dyn AnInjectable>().unwrap();
    let a = waypoint2.extract::<dyn AnInjectable>().unwrap();
    assert_ne!(&raw const *a, &raw const *b);

    let value7 =
        <dyn AnInjectable>::provision(&mut waypoint2, TypeId::of::<dyn ATag>().into()).expect("");
    waypoint2.provide(value7).err().unwrap();

    Waypoint::new_root::<dyn ATag>()
        .extract::<dyn AnInjectable>()
        .unwrap();
}
