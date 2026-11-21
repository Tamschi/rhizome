//! Small ephemeral infallible-ish lifetime-liftable backtracing pinned-dependency injection.
//!
//! Prefer to pass only exclusive borrows around, as [`Waypoint`] is potentially large (depending on the [`Repository`] it is instantiated with).
//!
//! # Panic-safety
//!
//! rhizome never panics (or errors) on its own, except out of memory, and escalates panics (and errors)
//! originating from [`Injectable::provision`] strictly to the direct caller.
//!
//! # Notes
//!
//! The drop order *and granularity* of resources is not specified until all related [`Waypoint`]s have been dropped.
//! Don't rely on it!
//!
//! There's no procedural provision API on [`Waypoint`], since it could easily lead to nondeterminism in conjunction with [`Future`]s.
//!
//! [`Repository`] is required to be `'static` because that simplifies lifting (but I'm open to suggestions).
//!
//! # TODO
//!
//! - A dynamically initialisable [`Repository`].
//! - Support for larger tuples?

#![warn(clippy::pedantic, clippy::style, missing_docs)]

use std::{
    alloc::Layout,
    any::{Any, TypeId},
    collections::BTreeMap,
    marker::PhantomData,
    mem::{self, ManuallyDrop, MaybeUninit},
    panic::{AssertUnwindSafe, catch_unwind, resume_unwind},
    pin::Pin,
    sync::{Arc, Mutex},
};

/// Hierarchical runtime dependency container
#[repr(transparent)] //TODO: Check if using an explicit layout is necessary.
pub struct Waypoint<'a, R: ?Sized>(Waypoint_<'a, R>);

mod sealed {

    use std::{any::TypeId, ffi::c_void, pin::Pin, sync::Arc};

    use crate::Injectable;

    pub(crate) unsafe trait Sealed {
        fn get(&self, key: TypeId) -> Option<*const Pin<Arc<c_void>>>;
    }

    unsafe impl<T: ?Sized + Sealed> Sealed for &'static T {
        fn get(&self, key: TypeId) -> Option<*const Pin<Arc<c_void>>> {
            <T as Sealed>::get(self, key)
        }
    }

    unsafe impl<T: ?Sized + Injectable> Sealed for Pin<Arc<T>> {
        fn get(&self, key: TypeId) -> Option<*const Pin<Arc<c_void>>> {
            (key == TypeId::of::<T>()).then_some((&raw const *self).cast())
        }
    }

    unsafe impl Sealed for () {
        fn get(&self, _key: TypeId) -> Option<*const Pin<Arc<c_void>>> {
            None
        }
    }

    unsafe impl<T0: ?Sized + Injectable> Sealed for (Pin<Arc<T0>>,) {
        fn get(&self, key: TypeId) -> Option<*const Pin<Arc<c_void>>> {
            (key == TypeId::of::<T0>()).then_some((&raw const self.0).cast())
        }
    }

    unsafe impl<T0: ?Sized + Injectable, T1: ?Sized + Injectable> Sealed
        for (Pin<Arc<T0>>, Pin<Arc<T1>>)
    {
        fn get(&self, key: TypeId) -> Option<*const Pin<Arc<c_void>>> {
            Some(match key {
                k if k == TypeId::of::<T0>() => (&raw const self.0).cast(),
                k if k == TypeId::of::<T1>() => (&raw const self.1).cast(),
                _ => return None,
            })
        }
    }

    unsafe impl<T0: ?Sized + Injectable, T1: ?Sized + Injectable, T2: ?Sized + Injectable> Sealed
        for (Pin<Arc<T0>>, Pin<Arc<T1>>, Pin<Arc<T2>>)
    {
        fn get(&self, key: TypeId) -> Option<*const Pin<Arc<c_void>>> {
            Some(match key {
                k if k == TypeId::of::<T0>() => (&raw const self.0).cast(),
                k if k == TypeId::of::<T1>() => (&raw const self.1).cast(),
                k if k == TypeId::of::<T2>() => (&raw const self.2).cast(),
                _ => return None,
            })
        }
    }

    unsafe impl<
        T0: ?Sized + Injectable,
        T1: ?Sized + Injectable,
        T2: ?Sized + Injectable,
        T3: ?Sized + Injectable,
    > Sealed for (Pin<Arc<T0>>, Pin<Arc<T1>>, Pin<Arc<T2>>, Pin<Arc<T3>>)
    {
        fn get(&self, key: TypeId) -> Option<*const Pin<Arc<c_void>>> {
            Some(match key {
                k if k == TypeId::of::<T0>() => (&raw const self.0).cast(),
                k if k == TypeId::of::<T1>() => (&raw const self.1).cast(),
                k if k == TypeId::of::<T2>() => (&raw const self.2).cast(),
                k if k == TypeId::of::<T3>() => (&raw const self.3).cast(),
                _ => return None,
            })
        }
    }

    unsafe impl<
        T0: ?Sized + Injectable,
        T1: ?Sized + Injectable,
        T2: ?Sized + Injectable,
        T3: ?Sized + Injectable,
        T4: ?Sized + Injectable,
    > Sealed
        for (
            Pin<Arc<T0>>,
            Pin<Arc<T1>>,
            Pin<Arc<T2>>,
            Pin<Arc<T3>>,
            Pin<Arc<T4>>,
        )
    {
        fn get(&self, key: TypeId) -> Option<*const Pin<Arc<c_void>>> {
            Some(match key {
                k if k == TypeId::of::<T0>() => (&raw const self.0).cast(),
                k if k == TypeId::of::<T1>() => (&raw const self.1).cast(),
                k if k == TypeId::of::<T2>() => (&raw const self.2).cast(),
                k if k == TypeId::of::<T3>() => (&raw const self.3).cast(),
                k if k == TypeId::of::<T4>() => (&raw const self.4).cast(),
                _ => return None,
            })
        }
    }

    unsafe impl<
        T0: ?Sized + Injectable,
        T1: ?Sized + Injectable,
        T2: ?Sized + Injectable,
        T3: ?Sized + Injectable,
        T4: ?Sized + Injectable,
        T5: ?Sized + Injectable,
    > Sealed
        for (
            Pin<Arc<T0>>,
            Pin<Arc<T1>>,
            Pin<Arc<T2>>,
            Pin<Arc<T3>>,
            Pin<Arc<T4>>,
            Pin<Arc<T5>>,
        )
    {
        fn get(&self, key: TypeId) -> Option<*const Pin<Arc<c_void>>> {
            Some(match key {
                k if k == TypeId::of::<T0>() => (&raw const self.0).cast(),
                k if k == TypeId::of::<T1>() => (&raw const self.1).cast(),
                k if k == TypeId::of::<T2>() => (&raw const self.2).cast(),
                k if k == TypeId::of::<T3>() => (&raw const self.3).cast(),
                k if k == TypeId::of::<T4>() => (&raw const self.4).cast(),
                k if k == TypeId::of::<T5>() => (&raw const self.5).cast(),
                _ => return None,
            })
        }
    }

    unsafe impl<
        T0: ?Sized + Injectable,
        T1: ?Sized + Injectable,
        T2: ?Sized + Injectable,
        T3: ?Sized + Injectable,
        T4: ?Sized + Injectable,
        T5: ?Sized + Injectable,
        T6: ?Sized + Injectable,
    > Sealed
        for (
            Pin<Arc<T0>>,
            Pin<Arc<T1>>,
            Pin<Arc<T2>>,
            Pin<Arc<T3>>,
            Pin<Arc<T4>>,
            Pin<Arc<T5>>,
            Pin<Arc<T6>>,
        )
    {
        fn get(&self, key: TypeId) -> Option<*const Pin<Arc<c_void>>> {
            Some(match key {
                k if k == TypeId::of::<T0>() => (&raw const self.0).cast(),
                k if k == TypeId::of::<T1>() => (&raw const self.1).cast(),
                k if k == TypeId::of::<T2>() => (&raw const self.2).cast(),
                k if k == TypeId::of::<T3>() => (&raw const self.3).cast(),
                k if k == TypeId::of::<T4>() => (&raw const self.4).cast(),
                k if k == TypeId::of::<T5>() => (&raw const self.5).cast(),
                k if k == TypeId::of::<T6>() => (&raw const self.6).cast(),
                _ => return None,
            })
        }
    }
}
use sealed::Sealed;

/// Marker trait for types that can act as node repositories.
#[expect(private_bounds)]
pub trait Repository: 'static + Sealed {}

impl Repository for () {}
impl<T: ?Sized + Repository> Repository for &'static T {}

impl<T: ?Sized + Injectable> Repository for Pin<Arc<T>> {}

impl<T0: ?Sized + Injectable> Repository for (Pin<Arc<T0>>,) {}

impl<T0: ?Sized + Injectable, T1: ?Sized + Injectable> Repository for (Pin<Arc<T0>>, Pin<Arc<T1>>) {}

impl<T0: ?Sized + Injectable, T1: ?Sized + Injectable, T2: ?Sized + Injectable> Repository
    for (Pin<Arc<T0>>, Pin<Arc<T1>>, Pin<Arc<T2>>)
{
}

impl<
    T0: ?Sized + Injectable,
    T1: ?Sized + Injectable,
    T2: ?Sized + Injectable,
    T3: ?Sized + Injectable,
> Repository for (Pin<Arc<T0>>, Pin<Arc<T1>>, Pin<Arc<T2>>, Pin<Arc<T3>>)
{
}

impl<
    T0: ?Sized + Injectable,
    T1: ?Sized + Injectable,
    T2: ?Sized + Injectable,
    T3: ?Sized + Injectable,
    T4: ?Sized + Injectable,
> Repository
    for (
        Pin<Arc<T0>>,
        Pin<Arc<T1>>,
        Pin<Arc<T2>>,
        Pin<Arc<T3>>,
        Pin<Arc<T4>>,
    )
{
}

impl<
    T0: ?Sized + Injectable,
    T1: ?Sized + Injectable,
    T2: ?Sized + Injectable,
    T3: ?Sized + Injectable,
    T4: ?Sized + Injectable,
    T5: ?Sized + Injectable,
> Repository
    for (
        Pin<Arc<T0>>,
        Pin<Arc<T1>>,
        Pin<Arc<T2>>,
        Pin<Arc<T3>>,
        Pin<Arc<T4>>,
        Pin<Arc<T5>>,
    )
{
}

impl<
    T0: ?Sized + Injectable,
    T1: ?Sized + Injectable,
    T2: ?Sized + Injectable,
    T3: ?Sized + Injectable,
    T4: ?Sized + Injectable,
    T5: ?Sized + Injectable,
    T6: ?Sized + Injectable,
> Repository
    for (
        Pin<Arc<T0>>,
        Pin<Arc<T1>>,
        Pin<Arc<T2>>,
        Pin<Arc<T3>>,
        Pin<Arc<T4>>,
        Pin<Arc<T5>>,
        Pin<Arc<T6>>,
    )
{
}

#[repr(C)]
enum Waypoint_<'a, R: ?Sized> {
    Inline(Inline<'a, R>),
    Lifted(Arc<Mutex<Lifted>>),
}

#[repr(C)]
struct Inline<'a, R: ?Sized> {
    next: InlineNext<'a>,
    tag: TypeId,
    provisioned: Option<Box<BTreeMap<TypeId, Raw>>>,
    //FIXME: Use static dispatch and unsize this struct inline once that's possible.
    repository: Box<dyn Repository>,
    _phantom: PhantomData<R>,
}

struct Lifted {
    next: Option<Arc<Mutex<Lifted>>>,
    tag: TypeId,
    provisioned: Option<Box<BTreeMap<TypeId, Raw>>>,
    repository: Box<dyn Repository>, //FIXME: Will be able to be `R`.
}

impl<R: Repository> Waypoint<'static, R> {
    /// Creates a new [`Waypoint`] hierarchy.
    pub fn new_root<T: ?Sized + Tag>(repository: R) -> Self {
        Self(Waypoint_::Inline(Inline::<R> {
            next: InlineNext::None,
            tag: TypeId::of::<T>(),
            provisioned: None,
            repository: Box::new(repository),
            _phantom: PhantomData,
        }))
    }
}

impl<R: ?Sized> Waypoint<'_, R> {
    /// Creates a new linked [`Waypoint`] falling back to `self`.
    pub fn new<T: ?Sized + Tag, R2>(&mut self, repository: R2) -> Waypoint<'_, R2>
    where
        R: Repository,
        R2: Repository,
    {
        Waypoint(Waypoint_::Inline(Inline {
            next: InlineNext::Waypoint(unsafe {
                mem::transmute::<&mut Waypoint<R>, &mut Waypoint<dyn Repository>>(self)
            }),
            tag: TypeId::of::<T>(),
            provisioned: None,
            repository: Box::new(repository),
            _phantom: PhantomData,
        }))
    }

    /// Creates a new `'static` [`Waypoint`] falling back to `self`'s state.
    ///
    /// This is potentially expensive (with persistent but idempotent effect on all ancestors).
    pub fn lift_and_new<T: ?Sized + Tag, R2>(&mut self, repository: R2) -> Waypoint<'static, R2>
    where
        R: Repository,
        R2: Repository,
    {
        Waypoint(Waypoint_::Inline(Inline {
            next: InlineNext::Lifted(match &self.0 {
                Waypoint_::Lifted(lifted) => Arc::clone(lifted),
                Waypoint_::Inline(_) => self.lift(),
            }),
            tag: TypeId::of::<T>(),
            provisioned: None,
            repository: Box::new(repository),
            _phantom: PhantomData,
        }))
    }

    fn lift(&mut self) -> Arc<Mutex<Lifted>>
    where
        R: Repository,
    {
        match &mut self.0 {
            Waypoint_::Inline(Inline {
                next,
                tag,
                provisioned,
                repository,
                _phantom: _,
            }) => {
                let mut uninit = Arc::new_uninit();
                let mutex = Mutex::new(Lifted {
                    next: match next {
                        InlineNext::Waypoint(waypoint) => Some(waypoint.lift()),
                        InlineNext::Lifted(_) => unreachable!("Lifting is transitive."),
                        InlineNext::None => None,
                    },
                    tag: *tag,
                    provisioned: provisioned.take(),
                    repository: mem::replace(repository, Box::new(())),
                });
                let lifted = unsafe {
                    Arc::get_mut(&mut uninit)
                        .unwrap_or_else(|| unreachable!())
                        .write(mutex);
                    uninit.assume_init()
                };
                self.0 = Waypoint_::Lifted(Arc::clone(&lifted));
                lifted
            }
            Waypoint_::Lifted(_) => unreachable!("Lifting is transitive."),
        }
    }

    /// Extracts a resource keyed with *exact* `T`.
    ///
    /// This method visits this and parent [`Waypoint`] data until (from highest to lowest priority)
    ///
    /// - an existing value is found at the current node,
    /// - <code>&lt;T as [`Injectable`]>::[PROVISION_AT](`Injectable::PROVISION_AT`)></code> includes its [`Tag`],
    /// - the root is reached (at which point provisioning is attempted without [`Tag`]).
    pub fn extract<T: ?Sized + Injectable>(&mut self) -> Result<T::Provided, T::Error>
    where
        R: Repository,
    {
        //TODO: This can be implemented iteratively. Somehow.

        let key = TypeId::of::<T>();
        match &mut self.0 {
            Waypoint_::Inline(Inline {
                next,
                tag,
                provisioned,
                repository,
                _phantom: _,
            }) => {
                if let Some(ptr) = repository.get(key) {
                    // Found
                    Ok(unsafe { T::provide(&*ptr.cast()) })
                } else if let Some(provisioned) = provisioned
                    && let Some(raw) = provisioned.get(&key)
                {
                    // Found
                    Ok(unsafe { T::provide(&raw.to_virtual_arc()) })
                } else if T::PROVISION_AT.contains(tag) {
                    // Tag match
                    let tag = *tag;
                    let raw = T::provision(self, Some(tag))?.into();
                    match &mut self.0 {
                        Waypoint_::Inline(inline) => Ok(unsafe {
                            T::provide(
                                &inline
                                    .provisioned
                                    .get_or_insert_default()
                                    .entry(key)
                                    .insert_entry(raw)
                                    .get()
                                    .to_virtual_arc(),
                            )
                        }),
                        Waypoint_::Lifted(lifted) => Ok(unsafe {
                            T::provide(
                                &lifted
                                    .lock()
                                    .unwrap_or_else(|_| unreachable!())
                                    .provisioned
                                    .get_or_insert_default()
                                    .entry(key)
                                    .insert_entry(raw)
                                    .get()
                                    .to_virtual_arc(),
                            )
                        }),
                    }
                } else {
                    // Next
                    match next {
                        InlineNext::Waypoint(waypoint) => waypoint.extract::<T>(),
                        InlineNext::Lifted(lifted) => {
                            Waypoint(Waypoint_::<dyn Repository>::Lifted(Arc::clone(lifted)))
                                .extract::<T>()
                        }
                        InlineNext::None => {
                            let raw = T::provision(self, None)?.into();
                            match &mut self.0 {
                                Waypoint_::Inline(inline) => Ok(unsafe {
                                    T::provide(
                                        &inline
                                            .provisioned
                                            .get_or_insert_default()
                                            .entry(key)
                                            .insert_entry(raw)
                                            .get()
                                            .to_virtual_arc(),
                                    )
                                }),
                                Waypoint_::Lifted(lifted) => Ok(unsafe {
                                    T::provide(
                                        &lifted
                                            .lock()
                                            .unwrap_or_else(|_| unreachable!())
                                            .provisioned
                                            .get_or_insert_default()
                                            .entry(key)
                                            .insert_entry(raw)
                                            .get()
                                            .to_virtual_arc(),
                                    )
                                }),
                            }
                        }
                    }
                }
            }
            Waypoint_::Lifted(lifted) => {
                let mut guard = lifted.lock().unwrap_or_else(|_| unreachable!());
                let result = catch_unwind(AssertUnwindSafe(|| {
                    let Lifted {
                        next,
                        tag,
                        provisioned,
                        repository,
                    } = &mut *guard;
                    if let Some(ptr) = repository.get(key) {
                        // Found
                        Ok(unsafe { T::provide(&*ptr.cast()) })
                    } else if let Some(provisioned) = provisioned
                        && let Some(raw) = provisioned.get(&key)
                    {
                        // Found
                        Ok(unsafe { T::provide(&raw.to_virtual_arc()) })
                    } else if T::PROVISION_AT.contains(tag) {
                        // Tag match
                        let raw: Raw = T::provision(
                            &mut Waypoint(Waypoint_::<dyn Repository>::Lifted(Arc::clone(&lifted))),
                            Some(*tag),
                        )?
                        .into();
                        Ok(unsafe {
                            T::provide(
                                &lifted
                                    .lock()
                                    .unwrap_or_else(|_| unreachable!())
                                    .provisioned
                                    .get_or_insert_default()
                                    .entry(key)
                                    .insert_entry(raw)
                                    .get()
                                    .to_virtual_arc(),
                            )
                        })
                    } else {
                        // Next
                        match next {
                            Some(lifted) => {
                                Waypoint(Waypoint_::<dyn Repository>::Lifted(Arc::clone(lifted)))
                                    .extract::<T>()
                            }
                            None => {
                                let raw = T::provision(
                                    &mut Waypoint(Waypoint_::<dyn Repository>::Lifted(Arc::clone(
                                        lifted,
                                    ))),
                                    None,
                                )?
                                .into();
                                Ok(unsafe {
                                    T::provide(
                                        &lifted
                                            .lock()
                                            .unwrap_or_else(|_| unreachable!())
                                            .provisioned
                                            .get_or_insert_default()
                                            .entry(key)
                                            .insert_entry(raw)
                                            .get()
                                            .to_virtual_arc(),
                                    )
                                })
                            }
                        }
                    }
                }));
                drop(guard);
                result.unwrap_or_else(|panic| resume_unwind(panic))
            }
        }
    }
}

enum InlineNext<'a> {
    Waypoint(&'a mut Waypoint<'a, dyn Repository>),
    Lifted(Arc<Mutex<Lifted>>),
    None,
}

struct Raw(
    MaybeUninit<*const dyn Any>,
    fn() -> &'static (
        unsafe fn(to_drop: MaybeUninit<*const dyn Any>),
        unsafe fn(to_increment: MaybeUninit<*const dyn Any>),
    ),
);

/// # Safety
///
/// Immutably wraps `Arc<dyn … + Send + Sync>`.
unsafe impl Send for Raw {}

/// # Safety
///
/// Immutably wraps `Arc<dyn … + Send + Sync>`.
unsafe impl Sync for Raw {}

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
    /// The resulting [`Arc`] must not be dropped.
    unsafe fn to_virtual_arc<T: ?Sized + Injectable>(&self) -> ManuallyDrop<Pin<Arc<T>>> {
        unsafe {
            ManuallyDrop::new(Pin::new_unchecked(Arc::from_raw(
                self.0.as_ptr().cast::<*const T>().read(),
            )))
        }
    }
}

/// Marks a type as potential [`Waypoint`] identity.
///
/// `dyn Tag` inherently implements this trait (so that unsized types may be used as [`Tag`]),
/// but it's not recommended to make use of this.
pub trait Tag: 'static {}

/// Marks a type as provision and extraction key.
pub trait Injectable: 'static + Send + Sync {
    /// While backtracing inside [`Waypoint::extract`],
    /// provisioning is automatically attempted at these-[`Tag`]ged nodes.
    const PROVISION_AT: &'static [TypeId];

    /// What is ultimately provided from [`Waypoint::extract`].
    ///
    /// Often, this should be <code>[`Pin`]&lt;[`Arc`]&lt;Self>></code> or <code>[`Arc`]&lt;Self></code>.
    type Provided;

    /// An error that may occur when trying to auto-provision the injectable value.
    type Error;

    /// Called to auto-provision `Self` (at a [`Waypoint`] with matched tag among [`Self::PROVISION_AT`] or the root [`Waypoint`]).
    ///
    /// Iff `Self` has `Self` as dependency and resolves without stack overflow,
    /// the outermost on "wins" and inner instances are successively released.\
    /// (This circumstance may raise a panic in future incompatible versions.)
    fn provision(
        at: &mut Waypoint<'_, impl ?Sized + Repository>,
        matched_tag: Option<TypeId>,
    ) -> Result<Pin<Arc<Self>>, Self::Error>;

    /// Derives the [`Self::Provided`] value from the provisioned value.
    fn provide(self: &Pin<Arc<Self>>) -> Self::Provided;
}
