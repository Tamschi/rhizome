#![warn(clippy::pedantic, clippy::style)]

use std::{
    alloc::Layout,
    any::{Any, TypeId},
    collections::{BTreeMap, btree_map::Entry},
    mem::{self, ManuallyDrop, MaybeUninit},
    panic::{AssertUnwindSafe, catch_unwind, resume_unwind},
    pin::Pin,
    sync::{Arc, Mutex},
};

use eros::TypeSet;

pub struct Waypoint<'a> {
    next: Next<'a>,
    managed: BTreeMap<TypeId, Raw>,
    tag: TypeId,
}

impl Waypoint<'static> {
    pub fn new_root<T: ?Sized + Tag>() -> Self {
        Self {
            next: Next::Stop(Arc::new(Mutex::new(BTreeMap::new()))),
            managed: BTreeMap::new(),
            tag: TypeId::of::<T>(),
        }
    }
}

impl Waypoint<'_> {
    pub fn new<T: ?Sized + Tag>(&mut self) -> Waypoint<'_> {
        Waypoint {
            next: Next::Waypoint(unsafe {
                //SAFETY: `next` is never written to.
                mem::transmute(self)
            }),
            managed: BTreeMap::new(),
            tag: TypeId::of::<T>(),
        }
    }

    pub fn squash<T: ?Sized + Tag>(&mut self) -> Waypoint<'static> {
        let mut managed = BTreeMap::new();
        let mut current = self;
        loop {
            for (&key, raw) in &current.managed {
                managed.entry(key).or_insert(raw.clone());
            }
            match &mut current.next {
                Next::Waypoint(next) => current = next,
                Next::Stop(arc) => {
                    break Waypoint {
                        next: Next::Stop(Arc::clone(arc)),
                        managed,
                        tag: TypeId::of::<T>(),
                    };
                }
            }
        }
    }

    pub fn inject<T: ?Sized + Injectable>(
        &mut self,
        value: Pin<Arc<T>>,
    ) -> Result<Pin<&mut T>, Pin<Arc<T>>> {
        let Entry::Vacant(entry) = self.managed.entry(TypeId::of::<T>()) else {
            return Err(value);
        };
        Ok({
            let raw = entry.insert(value.into());
            unsafe { raw.as_mut::<T>() }
        })
    }

    pub fn inject_with<T: ?Sized + Injectable, F: FnOnce() -> Pin<Arc<T>>>(
        &mut self,
        make_value: F,
    ) -> Result<Pin<&mut T>, F> {
        let Entry::Vacant(entry) = self.managed.entry(TypeId::of::<T>()) else {
            return Err(make_value);
        };
        Ok({
            let raw = entry.insert(make_value().into());
            unsafe { raw.as_mut::<T>() }
        })
    }

    pub fn extract<T: ?Sized + Injectable>(&mut self) -> eros::Result<Pin<Arc<T>>, T::ErrorSet> {
        let mut current = self;
        Ok(loop {
            let key = match current.managed.entry(TypeId::of::<T>()) {
                Entry::Occupied(entry) => {
                    break unsafe { entry.get().clone().into_arc() };
                }
                Entry::Vacant(entry) => {
                    if T::INJECT_AT.contains(&current.tag) {
                        let raw = entry.insert(T::provision(Some(current.tag))?.into());
                        break unsafe { raw.clone().into_arc() };
                    } else {
                        entry.into_key()
                    }
                }
            };

            match &mut current.next {
                Next::Waypoint(next) => current = next,
                Next::Stop(arc) => {
                    let mut lock = arc.lock().unwrap_or_else(|_| unreachable!());
                    let result = catch_unwind(AssertUnwindSafe(|| match lock.entry(key) {
                        Entry::Occupied(entry) => Ok(unsafe { entry.get().clone().into_arc() }),
                        Entry::Vacant(entry) => T::provision(None).map(|value| {
                            let raw = entry.insert(value.into());
                            unsafe { raw.clone().into_arc() }
                        }),
                    }));
                    drop(lock);
                    break result.unwrap_or_else(|panic| resume_unwind(panic))?;
                }
            }
        })
    }

    pub fn eject_local<T: ?Sized + Injectable>(&mut self) -> Option<Pin<Arc<T>>> {
        unsafe {
            self.managed
                .remove(&TypeId::of::<T>())
                .map(|raw| raw.into_arc())
        }
    }
}

enum Next<'a> {
    Waypoint(&'a mut Waypoint<'a>),
    Stop(Arc<Mutex<BTreeMap<TypeId, Raw>>>),
}

struct Raw(
    MaybeUninit<*const dyn Any>,
    fn() -> &'static (
        unsafe fn(to_drop: MaybeUninit<*const dyn Any>),
        unsafe fn(to_increment: MaybeUninit<*const dyn Any>),
    ),
);

impl Drop for Raw {
    fn drop(&mut self) {
        unsafe { self.1().0(self.0.clone()) }
    }
}

impl Clone for Raw {
    fn clone(&self) -> Self {
        unsafe { self.1().1(self.0.clone()) }
        Self(self.0.clone(), self.1.clone())
    }
}

impl<T: ?Sized + Injectable> From<Pin<Arc<T>>> for Raw {
    fn from(value: Pin<Arc<T>>) -> Self {
        const fn function_table<T: ?Sized>() -> &'static (
            unsafe fn(to_drop: MaybeUninit<*const dyn Any>),
            unsafe fn(to_increment: MaybeUninit<*const dyn Any>),
        ) {
            unsafe fn drop_value<T: ?Sized>(to_drop: MaybeUninit<*const dyn Any>) {
                unsafe {
                    let _ = Arc::from_raw(to_drop.as_ptr().cast::<*const T>().read());
                }
            }
            unsafe fn increment_counter<T: ?Sized>(to_increment: MaybeUninit<*const dyn Any>) {
                unsafe {
                    Arc::increment_strong_count(to_increment.as_ptr().cast::<*const T>().read());
                }
            }

            &(drop_value::<T>, increment_counter::<T>)
        }

        let mut data = MaybeUninit::<*const dyn Any>::uninit();
        unsafe {
            //SAFETY:
            const {
                let layout_t = Layout::new::<*const T>();
                let layout_r = Layout::new::<*const dyn Any>();
                assert!(layout_t.align() <= layout_r.align());
                assert!(layout_t.size() <= layout_r.size());
            }
            data.as_mut_ptr()
                .cast::<*const T>()
                .write(Arc::into_raw(Pin::into_inner_unchecked(value)));
        }
        Raw(data, function_table::<T>)
    }
}

impl Raw {
    /// # Safety
    ///
    /// - `Self` must have been converted from a `Pin<Arc<T>>`
    /// - that is being held exclusively.
    unsafe fn as_mut<T: ?Sized + Injectable>(&mut self) -> Pin<&mut T> {
        unsafe {
            let mut arc =
                ManuallyDrop::new(Arc::from_raw(self.0.as_ptr().cast::<*const T>().read()));
            let Some(borrow) = Arc::get_mut(&mut arc) else {
                unreachable!();
            };
            let borrow = Pin::new_unchecked(&mut *&raw mut *borrow);
            borrow
        }
    }

    unsafe fn into_arc<T: ?Sized + Injectable>(self) -> Pin<Arc<T>> {
        unsafe {
            let this = ManuallyDrop::new(self);
            Pin::new_unchecked(Arc::from_raw(this.0.as_ptr().cast::<*const T>().read()))
        }
    }
}

pub trait Tag: 'static {}

pub trait Injectable: 'static + Send + Sync {
    const INJECT_AT: &'static [TypeId];

    type ErrorSet: TypeSet;
    fn provision(tag: Option<TypeId>) -> eros::Result<Pin<Arc<Self>>, Self::ErrorSet>;
}
