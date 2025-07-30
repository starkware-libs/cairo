extern crate alloc;

use alloc::collections::VecDeque;
use alloc::vec::Vec;
use core::ops::{Deref, DerefMut};

/// A deque that implements `salsa::Update` if `T` implements `salsa::Update`.
/// This is needed to implement `salsa::Update` for `Deque<T>` results of queries.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Deque<T>(VecDeque<T>);

impl<T> Deref for Deque<T> {
    type Target = VecDeque<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> DerefMut for Deque<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[cfg(feature = "salsa")]
unsafe impl<T: salsa::Update> salsa::Update for Deque<T> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_deque: Self) -> bool {
        let new_deque = new_deque;
        let old_deque: &mut Self = unsafe { &mut *old_pointer };

        // If the lengths are different, just replace the old deque with the new one
        if old_deque.len() != new_deque.len() {
            old_deque.clear();
            old_deque.extend(new_deque);
            return true;
        }

        // Otherwise, recursively update each element
        let mut changed = false;
        for (old_item, new_item) in old_deque.iter_mut().zip(new_deque) {
            changed |= unsafe { T::maybe_update(old_item, new_item) };
        }
        changed
    }
}

impl<T> Default for Deque<T> {
    fn default() -> Self {
        Self(VecDeque::new())
    }
}

impl<T> Deque<T> {
    /// Creates a new empty deque.
    pub fn new() -> Self {
        Self(VecDeque::new())
    }

    /// Creates a new empty deque with space for at least `capacity` elements.
    pub fn with_capacity(capacity: usize) -> Self {
        Self(VecDeque::with_capacity(capacity))
    }
}

impl<T> IntoIterator for Deque<T> {
    type Item = T;
    type IntoIter = alloc::collections::vec_deque::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T> FromIterator<T> for Deque<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self(VecDeque::from_iter(iter))
    }
}

impl<T, const N: usize> From<[T; N]> for Deque<T> {
    fn from(arr: [T; N]) -> Self {
        Self(VecDeque::from(arr))
    }
}

impl<T> From<Vec<T>> for Deque<T> {
    fn from(vec: Vec<T>) -> Self {
        Self(VecDeque::from(vec))
    }
}

impl<T> From<Deque<T>> for Vec<T> {
    fn from(deque: Deque<T>) -> Self {
        deque.0.into()
    }
}
