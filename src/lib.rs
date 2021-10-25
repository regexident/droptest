// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! A helper crate for testing drop-semantics (i.e. whether or not a value was or as not dropped as expected).
//!
//! # Examples
//!
//! Ensure [`std::mem::drop`](http://doc.rust-lang.org/1.56.0/std/mem/fn.drop.html) does in fact drop:
//!
//! ```
//! use droptest::prelude::*;
//!
//! let registry = DropRegistry::default();
//! let guard = registry.new_guard();
//! let guard_id = guard.id();
//!
//! std::mem::drop(guard);
//! assert_drop!(registry, guard_id);
//! ```
//!
//! Ensure [`std::mem::forget`](https://doc.rust-lang.org/stable/std/mem/fn.forget.html) does not drop:
//!
//! ```
//! use droptest::prelude::*;
//!
//! let registry = DropRegistry::default();
//! let guard = registry.new_guard();
//! let guard_id = guard.id();
//!
//! std::mem::forget(guard);
//! assert_no_drop!(registry, guard_id);
//! ```
//!
//! Ensure [`std::rc::Rc`](https://doc.rust-lang.org/stable/std/rc/struct.Rc.html)
//! only drops the when reference count reaches `0`:
//!
//! ```
//! use droptest::prelude::*;
//!
//! let registry = DropRegistry::default();
//! let guard = registry.new_guard();
//! let guard_id = guard.id();
//!
//! let rc = std::rc::Rc::new(guard);
//! let rc_clone = rc.clone();
//!
//! std::mem::drop(rc);
//! assert_no_drop!(registry, guard_id);
//!
//! std::mem::drop(rc_clone);
//! assert_drop!(registry, guard_id);
//! ```

use std::{
    fmt,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    sync::Mutex,
};

/// The doptest prelude.
pub mod prelude {
    pub use super::{
        assert_drop, assert_drop_stats, assert_no_drop, DropGuard, DropGuardId, DropRegistry,
        DropStatistics,
    };
}

#[macro_export]
macro_rules! assert_drop {
    ($registry:expr, $guard_id:expr) => {
        assert!($registry.is_dropped($guard_id), "expected drop for {:?}.", $guard_id);
    };
    ($registry:expr, $guard_id:expr $(,$message:tt)+) => {
        assert!($registry.is_dropped($guard_id), "expected drop for {:?}: {}", $guard_id, format!($($message)+));
    };
}

#[macro_export]
macro_rules! assert_no_drop {
    ($registry:expr, $guard_id:expr) => {
        assert!(!$registry.is_dropped($guard_id), "expected no drop for {:?}.", $guard_id);
    };
    ($registry:expr, $guard_id:expr $(,$message:tt)*) => {
        assert!(!$registry.is_dropped($guard_id), "expected no drop for {:?}: {}", $guard_id, format!($($message)+));
    };
}

#[macro_export]
macro_rules! assert_drop_stats {
    ($registry:expr, { $($field:ident: $expected:expr),+ }) => {
        assert!(matches!($registry.stats(), $crate::DropStatistics {
            $($field: $expected,)*
            ..
        }), concat!("expected {{ ", stringify!($($field: $expected),*), " }}"));
    };
    ($registry:expr, { $($field:ident: $expected:expr),+ } $(,$message:tt)*) => {
        assert!(matches!($registry.stats(), $crate::DropStatistics {
            $($field: $expected,)*
            ..
        }), concat!("expected {{ ", stringify!($($field: $expected),*), " }}: {}"), format!($($message)+));
    };
}

/// An identifier associated with a registry's individual registered guards.
#[derive(Default, Eq, PartialEq, Ord, PartialOrd, Copy, Clone)]
pub struct DropGuardId<'a> {
    value: usize,
    _phantom: PhantomData<&'a DropRegistry>,
}

impl<'a> DropGuardId<'a> {
    fn new(value: usize) -> Self {
        Self {
            value,
            _phantom: PhantomData,
        }
    }
}

impl<'a> fmt::Debug for DropGuardId<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

/// A guard object that reports to its registry when it gets dropped.
pub struct DropGuard<'a, T> {
    id: DropGuardId<'a>,
    value: T,
    registry: &'a DropRegistry,
}

impl<'a, T> DropGuard<'a, T> {
    /// Returns the id associated with the guard object.
    ///
    /// # Examples
    /// ```
    /// use droptest::prelude::*;
    ///
    /// let registry = DropRegistry::default();
    /// let guard = registry.new_guard();
    /// let id = guard.id();
    ///
    /// assert_no_drop!(registry, id);
    /// ```
    #[inline]
    pub fn id(&self) -> DropGuardId<'a> {
        self.id
    }

    /// Returns the id associated with the guard object.
    ///
    /// # Examples
    /// ```
    /// use droptest::prelude::*;
    ///
    /// let registry = DropRegistry::default();
    /// let guard = registry.new_guard_for(42);
    /// let value = guard.value();
    ///
    /// assert_eq!(value, &42);
    /// ```
    #[inline]
    pub fn value(&self) -> &T {
        &self.value
    }

    /// Returns `(self.id(), self)`.
    ///
    /// # Examples
    /// ```
    /// use droptest::prelude::*;
    ///
    /// let registry = DropRegistry::default();
    /// let (id, guard) = registry.new_guard_for(42).by_id();
    ///
    /// assert_no_drop!(registry, id);
    /// ```
    #[inline]
    pub fn by_id(self) -> (DropGuardId<'a>, Self) {
        (self.id(), self)
    }

    /// Returns `true` if the guard is registered with a given registry, otherwise `false`.
    ///
    /// # Examples
    /// ```
    /// use droptest::prelude::*;
    ///
    /// let registry = DropRegistry::default();
    /// let guard = registry.new_guard();
    /// let id = guard.id();
    ///
    /// assert!(guard.is_registered_to(&registry));
    /// ```
    #[inline]
    pub fn is_registered_to(&self, registry: &DropRegistry) -> bool {
        std::ptr::eq(self.registry, registry)
    }
}

impl<'a, T> Deref for DropGuard<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<'a, T> DerefMut for DropGuard<'a, T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<'a, T> PartialEq for DropGuard<'a, T>
where
    T: PartialEq,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }
}

impl<'a, T> PartialOrd for DropGuard<'a, T>
where
    T: PartialOrd,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(&other.value)
    }
}

impl<'a, T> Clone for DropGuard<'a, T>
where
    T: Clone,
{
    #[inline]
    fn clone(&self) -> Self {
        self.registry.new_guard_for(self.value.clone())
    }
}

impl<'a, T> Drop for DropGuard<'a, T> {
    #[inline]
    fn drop(&mut self) {
        self.registry.on_drop(self.id)
    }
}

impl<'a, T> fmt::Debug for DropGuard<'a, T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("DropGuard")
            .field("id", &self.id)
            .field("value", &self.value)
            .finish()
    }
}

/// A snapshot for a registry's statistics at a given time.
#[derive(Default, Eq, PartialEq, Debug)]
pub struct DropStatistics {
    /// Number of guards created by the corresponding registry.
    pub created: usize,
    /// Number of guards dropped by the corresponding registry.
    pub dropped: usize,
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
enum DropGuardState {
    Alive,
    Dropped,
}

unsafe impl Send for DropGuardState {}
unsafe impl Sync for DropGuardState {}

/// A registry for tracking a guard's liveness state (i.e. alive vs. dropped).
#[derive(Default, Debug)]
pub struct DropRegistry {
    guard_states: Mutex<Vec<DropGuardState>>,
}

impl DropRegistry {
    /// Returns a registered wrapper for a provided value.
    ///
    /// # Examples
    /// ```
    /// use droptest::prelude::*;
    ///
    /// let registry = DropRegistry::default();
    /// let guard = registry.new_guard_for(42);
    /// let value = guard.value();
    ///
    /// assert_eq!(value, &42);
    /// ```
    pub fn new_guard_for<T>(&self, value: T) -> DropGuard<'_, T> {
        let guard_states = &mut self.guard_states.lock().unwrap();

        let id = DropGuardId::new(guard_states.len());
        guard_states.push(DropGuardState::Alive);

        let registry = self;

        DropGuard {
            id,
            value,
            registry,
        }
    }

    /// Returns a registered wrapper for a `()` value.
    ///
    /// # Examples
    /// ```
    /// use droptest::prelude::*;
    ///
    /// let registry = DropRegistry::default();
    /// let guard = registry.new_guard();
    /// let value = guard.value();
    ///
    /// assert_eq!(value, &());
    /// ```
    #[inline]
    pub fn new_guard(&self) -> DropGuard<'_, ()> {
        self.new_guard_for(())
    }

    /// Returns the statistics snapshot for the registry at the current time.
    ///
    /// # Examples
    /// ```
    /// use droptest::prelude::*;
    ///
    /// let registry = DropRegistry::default();
    /// let guard = registry.new_guard();
    /// let stats = registry.stats();
    ///
    /// assert_eq!(stats, DropStatistics {
    ///     created: 1,
    ///     dropped: 0
    /// });
    /// ```
    pub fn stats(&self) -> DropStatistics {
        let guard_states = &self.guard_states.lock().unwrap();
        let mut stats = DropStatistics::default();
        for guard_state in guard_states.iter() {
            stats.created += 1;
            match *guard_state {
                DropGuardState::Alive => {}
                DropGuardState::Dropped => stats.dropped += 1,
            }
        }
        stats
    }

    /// Returns `true` if the guard associated with `id` has been dropped, otherwise `false.
    ///
    /// # Examples
    /// ```
    /// use droptest::prelude::*;
    ///
    /// let registry = DropRegistry::default();
    /// let guard = registry.new_guard();
    /// let guard_id = guard.id();
    ///
    /// assert!(!registry.is_dropped(guard_id));
    ///
    /// std::mem::drop(guard);
    ///
    /// assert!(registry.is_dropped(guard_id));
    /// ```
    #[inline]
    pub fn is_dropped<'a>(&'a self, id: DropGuardId<'a>) -> bool {
        let guard_states = &self.guard_states.lock().unwrap();
        let id_value = id.value;
        guard_states[id_value] == DropGuardState::Dropped
    }

    fn on_drop<'a>(&'a self, id: DropGuardId<'a>) {
        let guard_states = &mut self.guard_states.lock().unwrap();
        let id_value = id.value;
        if guard_states[id_value] == DropGuardState::Dropped {
            panic!("DropGuard {:?} has already been dropped", id_value);
        }
        guard_states[id_value] = DropGuardState::Dropped;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let registry = DropRegistry::default();

        assert_drop_stats!(registry, { created: 0, dropped: 0 });

        let guard = registry.new_guard_for(42);

        assert_drop_stats!(registry, { created: 1, dropped: 0 });

        std::mem::drop(guard);

        assert_drop_stats!(registry, { created: 1, dropped: 1 });
    }

    #[test]
    fn is_registered_to() {
        let registry = DropRegistry::default();
        let other_registry = DropRegistry::default();

        let guard = registry.new_guard_for(42);

        assert!(guard.is_registered_to(&registry));
        assert!(!guard.is_registered_to(&other_registry));
    }

    #[test]
    fn is_dropped() {
        let registry = DropRegistry::default();

        let guard = registry.new_guard_for(42);
        let guard_id = guard.id();

        assert_no_drop!(registry, guard_id);

        std::mem::drop(guard);

        assert_drop!(registry, guard_id);
    }

    #[test]
    #[should_panic]
    fn double_drop_panics() {
        let registry = DropRegistry::default();

        let guard = registry.new_guard_for(42);
        registry.on_drop(guard.id());

        std::mem::drop(guard);
    }

    #[test]
    fn clone() {
        let registry = DropRegistry::default();

        assert_drop_stats!(registry, { created: 0, dropped: 0 });

        let guard = registry.new_guard_for(42);
        let guard_id = guard.id();

        assert_drop_stats!(registry, { created: 1, dropped: 0 });

        let cloned_guard = guard.clone();
        let cloned_guard_id = cloned_guard.id();

        assert_drop_stats!(registry, { created: 2, dropped: 0 });

        std::mem::drop(guard);

        assert_drop!(registry, guard_id);
        assert_no_drop!(registry, cloned_guard_id);
        assert_drop_stats!(registry, { created: 2, dropped: 1 });

        std::mem::drop(cloned_guard);

        assert_drop!(registry, guard_id);
        assert_drop!(registry, cloned_guard_id);
        assert_drop_stats!(registry, { created: 2, dropped: 2 });
    }

    #[test]
    fn assert_should_not_panic() {
        let registry = DropRegistry::default();
        let (id, guard) = registry.new_guard().by_id();

        assert_no_drop!(registry, id);

        std::mem::drop(guard);

        assert_drop!(registry, id);
    }

    #[test]
    #[should_panic]
    fn assert_no_drop_should_panic() {
        let registry = DropRegistry::default();
        let (id, guard) = registry.new_guard().by_id();

        std::mem::drop(guard);

        assert_no_drop!(registry, id);
    }

    #[test]
    #[should_panic]
    fn assert_drop_should_panic() {
        let registry = DropRegistry::default();
        let (id, guard) = registry.new_guard().by_id();

        assert_drop!(registry, id);

        std::mem::drop(guard);
    }

    #[test]
    fn std_mem_drop() {
        let registry = DropRegistry::default();
        let guard = registry.new_guard();
        let guard_id = guard.id();

        std::mem::drop(guard);

        assert_drop!(registry, guard_id);
    }

    #[test]
    fn std_mem_forget() {
        let registry = DropRegistry::default();
        let guard = registry.new_guard();
        let guard_id = guard.id();

        std::mem::forget(guard);

        assert_no_drop!(registry, guard_id);
    }

    #[test]
    fn std_sync_rc() {
        let registry = DropRegistry::default();
        let guard = registry.new_guard();
        let guard_id = guard.id();

        let rc = std::rc::Rc::new(guard);
        let rc_clone = rc.clone();

        std::mem::drop(rc);

        assert_no_drop!(registry, guard_id);

        std::mem::drop(rc_clone);

        assert_drop!(registry, guard_id);
    }
}
