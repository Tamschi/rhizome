use std::{any::TypeId, pin::Pin, sync::Arc};

use rhizome::{Injectable, Repository, Tag, Waypoint};

trait ATag {}
impl Tag for dyn ATag {}

trait AnInjectable: Send + Sync {}
impl Injectable for dyn AnInjectable {
    const PROVISION_AT: &'static [TypeId] = &[];

    type Error = !;

    fn provision(
        _at: &mut Waypoint<'_, impl ?Sized + Repository>,
        _matched_tag: Option<TypeId>,
    ) -> Result<Pin<Arc<Self>>, Self::Error> {
        Ok(Arc::pin(Struct))
    }

    type Provided = Pin<Arc<Self>>;

    fn provide(self: &Pin<Arc<Self>>) -> Self::Provided {
        Pin::clone(self)
    }
}
struct Struct;
impl AnInjectable for Struct {}

#[test]
fn demo() {
    let mut waypoint =
        Waypoint::new_root::<dyn ATag>(Arc::pin(Struct) as Pin<Arc<dyn AnInjectable>>);

    let mut waypoint = waypoint.new::<dyn ATag, _>(());
    let mut waypoint2 = waypoint.lift_and_new::<dyn ATag, _>(());
    drop(waypoint);

    let mut waypoint =
        waypoint2.new::<dyn ATag, _>((Arc::pin(Struct) as Pin<Arc<dyn AnInjectable>>,));
    let b = waypoint.extract::<dyn AnInjectable>().unwrap();
    let a = waypoint2.extract::<dyn AnInjectable>().unwrap();
    assert_ne!(&raw const *a, &raw const *b);

    Waypoint::new_root::<dyn ATag>(())
        .extract::<dyn AnInjectable>()
        .unwrap();
}
