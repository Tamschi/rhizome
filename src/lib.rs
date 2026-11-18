//! Small ephemeral infallible-ish lifetime-liftable backtracing pinned-dependency injection.
//!
//! Prefer to pass only exclusive borrows around, as [`Waypoint`] is a few pointers wide.
//!
//! # Panic-safety
//!
//! rhizome never panics (or errors) on its own and escalates panics (and errors) originating from
//! [`Injectable::provision`] and inside [`Waypoint::provide_with`] strictly to the direct caller.

#![warn(clippy::pedantic, clippy::style, missing_docs)]

use std::{
    alloc::Layout,
    any::{Any, TypeId},
    collections::{BTreeMap, btree_map::Entry},
    mem::{self, ManuallyDrop, MaybeUninit},
    ops::{Deref, DerefMut},
    panic::{AssertUnwindSafe, catch_unwind, resume_unwind},
    pin::Pin,
    ptr::NonNull,
    sync::{Arc, Mutex, MutexGuard},
};

use eros::TypeSet;

/// Hierarchical runtime dependency container
pub struct Waypoint<'a>(Waypoint_<'a>);

enum Waypoint_<'a> {
    Inline(Inline<'a>),
    Lifted(Arc<Mutex<Lifted>>),
}

struct Inline<'a> {
    next: InlineNext<'a>,
    managed: BTreeMap<TypeId, Raw>,
    tag: TypeId,
}

struct Lifted {
    next: LiftedNext,
    managed: BTreeMap<TypeId, Raw>,
    tag: TypeId,
}

impl Waypoint<'static> {
    /// Creates a new [`Waypoint`] hierarchy.
    pub fn new_root<T: ?Sized + Tag>() -> Self {
        Self(Waypoint_::Inline(Inline {
            next: InlineNext::Stop(Box::new(BTreeMap::new())),
            managed: BTreeMap::new(),
            tag: TypeId::of::<T>(),
        }))
    }
}

enum DerefEither<Left, Right> {
    Left(Left),
    Right(Right),
}

impl<L, R> Deref for DerefEither<L, R>
where
    L: Deref,
    R: Deref<Target = L::Target>,
{
    type Target = L::Target;

    fn deref(&self) -> &Self::Target {
        match self {
            DerefEither::Left(left) => left,
            DerefEither::Right(right) => right,
        }
    }
}

impl<L, R> DerefMut for DerefEither<L, R>
where
    L: DerefMut,
    R: DerefMut<Target = L::Target>,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            DerefEither::Left(left) => left,
            DerefEither::Right(right) => right,
        }
    }
}

/// # Safety
///
/// `projected` must remain valid for `'a`.
unsafe fn projected<'a, T: ?Sized, U: ?Sized>(
    guard: MutexGuard<'a, T>,
    projected: NonNull<U>,
) -> impl DerefMut<Target = U> {
    struct Projection<'a, T: ?Sized, U: ?Sized>(MutexGuard<'a, T>, NonNull<U>);

    impl<'a, T: ?Sized, U: ?Sized> Deref for Projection<'a, T, U> {
        type Target = U;

        fn deref(&self) -> &Self::Target {
            unsafe { self.1.as_ref() }
        }
    }

    impl<'a, T: ?Sized, U: ?Sized> DerefMut for Projection<'a, T, U> {
        fn deref_mut(&mut self) -> &mut Self::Target {
            unsafe { self.1.as_mut() }
        }
    }

    Projection(guard, projected)
}

impl Waypoint<'_> {
    /// Creates a new linked [`Waypoint`] falling back to `self`.
    pub fn new<T: ?Sized + Tag>(&mut self) -> Waypoint<'_> {
        Waypoint(Waypoint_::Inline(Inline {
            next: InlineNext::Waypoint(unsafe { mem::transmute(self) }),
            managed: BTreeMap::new(),
            tag: TypeId::of::<T>(),
        }))
    }

    /// Creates a new `'static` [`Waypoint`] falling back to `self`'s state.
    ///
    /// This is potentially expensive (with persistent but idempotent effect on all ancestors).
    pub fn lift_and_new<T: ?Sized + Tag>(&mut self) -> Waypoint<'static> {
        Waypoint(Waypoint_::Inline(Inline {
            next: InlineNext::Lifted(match &self.0 {
                Waypoint_::Lifted(lifted) => Arc::clone(lifted),
                Waypoint_::Inline(_) => self.lift(),
            }),
            managed: BTreeMap::new(),
            tag: TypeId::of::<T>(),
        }))
    }

    fn lift(&mut self) -> Arc<Mutex<Lifted>> {
        match &mut self.0 {
            Waypoint_::Inline(Inline { next, managed, tag }) => {
                let mut uninit = Arc::new_uninit();
                let mutex = Mutex::new(Lifted {
                    next: match next {
                        InlineNext::Waypoint(waypoint) => LiftedNext::Lifted(waypoint.lift()),
                        InlineNext::Lifted(_) => unreachable!("Lifting is transitive."),
                        InlineNext::Stop(root) => LiftedNext::Stop(mem::take(&mut *root)),
                    },
                    managed: mem::take(managed),
                    tag: *tag,
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

    /// Iff not locally occupied,
    /// provides `value` for extractions of `T` at `self`
    /// and (where not shadowed) its children.
    pub fn provide<T: ?Sized + Injectable>(
        &mut self,
        value: Pin<Arc<T>>,
    ) -> Result<Pin<impl DerefMut<Target = T>>, Pin<Arc<T>>> {
        match &mut self.0 {
            Waypoint_::Inline(inline) => {
                let Entry::Vacant(entry) = inline.managed.entry(TypeId::of::<T>()) else {
                    return Err(value);
                };
                Ok({
                    let raw = entry.insert(value.into());
                    unsafe {
                        Pin::new_unchecked(DerefEither::Left(Pin::into_inner_unchecked(
                            raw.as_mut::<T>(),
                        )))
                    }
                })
            }
            Waypoint_::Lifted(lifted) => {
                let mut guard = lifted.lock().unwrap_or_else(|_| unreachable!());
                let Entry::Vacant(entry) = guard.managed.entry(TypeId::of::<T>()) else {
                    return Err(value);
                };
                Ok({
                    let raw = entry.insert(value.into());
                    unsafe {
                        let ptr = raw.as_mut_ptr();
                        Pin::new_unchecked(DerefEither::Right(projected(guard, ptr)))
                    }
                })
            }
        }
    }

    /// Iff not locally occupied,
    /// provides the return value of `make_value` for extractions of `T` at `self`
    /// and (where not shadowed) its children.
    pub fn provide_with<T: ?Sized + Injectable, F: FnOnce() -> Pin<Arc<T>>>(
        &mut self,
        make_value: F,
    ) -> Result<Pin<impl DerefMut<Target = T>>, F> {
        match &mut self.0 {
            Waypoint_::Inline(inline) => {
                let Entry::Vacant(entry) = inline.managed.entry(TypeId::of::<T>()) else {
                    return Err(make_value);
                };
                Ok({
                    let raw = entry.insert(make_value().into());
                    unsafe {
                        Pin::new_unchecked(DerefEither::Left(Pin::into_inner_unchecked(
                            raw.as_mut::<T>(),
                        )))
                    }
                })
            }
            Waypoint_::Lifted(lifted) => {
                let mut guard = lifted.lock().unwrap_or_else(|_| unreachable!());
                let Entry::Vacant(entry) = guard.managed.entry(TypeId::of::<T>()) else {
                    return Err(make_value);
                };
                Ok({
                    let raw = entry.insert(make_value().into());
                    unsafe {
                        let ptr = raw.as_mut_ptr();
                        Pin::new_unchecked(DerefEither::Right(projected(guard, ptr)))
                    }
                })
            }
        }
    }

    /// Iff not locally occupied,
    /// tries to provide the [`Ok`] return value of `make_value` for extractions of `T` at `self`
    /// and (where not shadowed) its children.
    pub fn try_provide_with<T: ?Sized + Injectable, E, F: FnOnce() -> Result<Pin<Arc<T>>, E>>(
        &mut self,
        make_value: F,
    ) -> Result<Result<Pin<impl DerefMut<Target = T>>, F>, E> {
        match &mut self.0 {
            Waypoint_::Inline(inline) => {
                let Entry::Vacant(entry) = inline.managed.entry(TypeId::of::<T>()) else {
                    return Ok(Err(make_value));
                };
                make_value().map(|value| {
                    Ok({
                        let raw = entry.insert(value.into());
                        unsafe {
                            Pin::new_unchecked(DerefEither::Left(Pin::into_inner_unchecked(
                                raw.as_mut::<T>(),
                            )))
                        }
                    })
                })
            }
            Waypoint_::Lifted(lifted) => {
                let mut guard = lifted.lock().unwrap_or_else(|_| unreachable!());
                let Entry::Vacant(entry) = guard.managed.entry(TypeId::of::<T>()) else {
                    return Ok(Err(make_value));
                };
                Ok(Ok({
                    let raw = entry.insert(make_value()?.into());
                    unsafe {
                        let ptr = raw.as_mut_ptr();
                        Pin::new_unchecked(DerefEither::Right(projected(guard, ptr)))
                    }
                }))
            }
        }
    }

    /// Extracts a resource keyed with *exact* `T`.
    ///
    /// This method visits this and parent [`Waypoint`] data until (from highest to lowest priority)
    ///
    /// - an existing value is found at the current node,
    /// - <code>&lt;T as [`Injectable`]>::[PROVISION_AT](`Injectable::PROVISION_AT`)></code> includes its [`Tag`],
    /// - the root is reached (at which point provisioning is attempted without [`Tag`]).
    pub fn extract<T: ?Sized + Injectable>(&mut self) -> eros::Result<T::Provided, T::ErrorSet> {
        let mut current = self;

        let lifted = loop {
            match &mut current.0 {
                Waypoint_::Inline(inline) => {
                    let key = match inline.managed.entry(TypeId::of::<T>()) {
                        Entry::Occupied(entry) => {
                            return Ok(unsafe { T::provide(&entry.get().to_virtual_arc()) });
                        }
                        Entry::Vacant(entry) => {
                            if T::PROVISION_AT.contains(&inline.tag) {
                                let raw = entry.insert(T::provision(Some(inline.tag))?.into());
                                return Ok(unsafe { T::provide(&raw.to_virtual_arc()) });
                            } else {
                                entry.into_key()
                            }
                        }
                    };

                    match &mut inline.next {
                        InlineNext::Waypoint(next) => current = next,
                        InlineNext::Lifted(lifted) => break lifted,
                        InlineNext::Stop(root) => {
                            return match root.entry(key) {
                                Entry::Occupied(entry) => {
                                    Ok(unsafe { T::provide(&entry.get().to_virtual_arc()) })
                                }
                                Entry::Vacant(entry) => T::provision(None).map(|value| {
                                    let raw = entry.insert(value.into());
                                    unsafe { T::provide(&raw.to_virtual_arc()) }
                                }),
                            };
                        }
                    }
                }
                Waypoint_::Lifted(lifted) => break lifted,
            }
        };

        let mut guard = lifted.lock().unwrap_or_else(|_| unreachable!());
        let result = catch_unwind(AssertUnwindSafe(|| guard.extract::<T>()));
        drop(guard);
        result.unwrap_or_else(|panic| resume_unwind(panic))
    }
}

impl Lifted {
    pub fn extract<T: ?Sized + Injectable>(&mut self) -> eros::Result<T::Provided, T::ErrorSet> {
        let key = match self.managed.entry(TypeId::of::<T>()) {
            Entry::Occupied(entry) => {
                return Ok(unsafe { T::provide(&entry.get().to_virtual_arc()) });
            }
            Entry::Vacant(entry) => {
                if T::PROVISION_AT.contains(&self.tag) {
                    let raw = entry.insert(T::provision(Some(self.tag))?.into());
                    return Ok(unsafe { T::provide(&raw.to_virtual_arc()) });
                } else {
                    entry.into_key()
                }
            }
        };

        let lifted = match &mut self.next {
            LiftedNext::Lifted(lifted) => lifted,
            LiftedNext::Stop(root) => {
                return match root.entry(key) {
                    Entry::Occupied(entry) => {
                        Ok(unsafe { T::provide(&entry.get().to_virtual_arc()) })
                    }
                    Entry::Vacant(entry) => T::provision(None).map(|value| {
                        let raw = entry.insert(value.into());
                        unsafe { T::provide(&raw.to_virtual_arc()) }
                    }),
                };
            }
        };

        let mut guard = lifted.lock().unwrap_or_else(|_| unreachable!());
        let result = catch_unwind(AssertUnwindSafe(|| guard.extract::<T>()));
        drop(guard);
        result.unwrap_or_else(|panic| resume_unwind(panic))
    }
}

enum InlineNext<'a> {
    Waypoint(&'a mut Waypoint<'a>),
    Lifted(Arc<Mutex<Lifted>>),
    Stop(Box<BTreeMap<TypeId, Raw>>),
}

enum LiftedNext {
    Lifted(Arc<Mutex<Lifted>>),
    Stop(Box<BTreeMap<TypeId, Raw>>),
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

    /// # Safety
    ///
    /// - `Self` must have been converted from a `Pin<Arc<T>>`
    /// - that is being held exclusively.
    unsafe fn as_mut_ptr<T: ?Sized + Injectable>(&mut self) -> NonNull<T> {
        unsafe {
            let mut arc =
                ManuallyDrop::new(Arc::from_raw(self.0.as_ptr().cast::<*const T>().read()));
            let Some(borrow) = Arc::get_mut(&mut arc) else {
                unreachable!();
            };
            NonNull::from_mut(borrow)
        }
    }

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

    /// An [`eros::TypeSet`] that determines how and whether [`Waypoint::extract`] may fail due to a provisioning failure.
    ///
    /// Usually, this will likely be [`()`], [`eros::AnyError`] or a tuple of specific error types
    /// that may occur in the returned [`eros::ErrorUnion`].
    type ErrorSet: TypeSet;

    //TODO: Allow this to pull in further dependencies.
    fn provision(tag: Option<TypeId>) -> eros::Result<Pin<Arc<Self>>, Self::ErrorSet>;

    /// Derives the [`Self::Provided`] value from the provisioned value.
    fn provide(self: &Pin<Arc<Self>>) -> Self::Provided;
}
