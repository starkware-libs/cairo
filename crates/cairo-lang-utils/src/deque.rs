#[cfg(not(feature = "std"))]
extern crate alloc;

use core::ops::{Index, IndexMut};

// Type aliases for conditional std/no-std support
#[cfg(feature = "std")]
type VecDeque<T> = std::collections::VecDeque<T>;

#[cfg(not(feature = "std"))]
type VecDeque<T> = alloc::collections::VecDeque<T>;

#[cfg(feature = "std")]
type Vec<T> = std::vec::Vec<T>;

#[cfg(not(feature = "std"))]
type Vec<T> = alloc::vec::Vec<T>;

#[cfg(feature = "std")]
type IterImpl<'a, T> = std::collections::vec_deque::Iter<'a, T>;

#[cfg(not(feature = "std"))]
type IterImpl<'a, T> = alloc::collections::vec_deque::Iter<'a, T>;

#[cfg(feature = "std")]
type IterMutImpl<'a, T> = std::collections::vec_deque::IterMut<'a, T>;

#[cfg(not(feature = "std"))]
type IterMutImpl<'a, T> = alloc::collections::vec_deque::IterMut<'a, T>;

#[cfg(feature = "std")]
type IntoIterImpl<T> = std::collections::vec_deque::IntoIter<T>;

#[cfg(not(feature = "std"))]
type IntoIterImpl<T> = alloc::collections::vec_deque::IntoIter<T>;

#[derive(Clone, Debug)]
pub struct Deque<T>(VecDeque<T>);

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

    /// Returns the number of elements in the deque.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if the deque is empty.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Removes all elements from the deque.
    pub fn clear(&mut self) {
        self.0.clear()
    }

    /// Provides a reference to the front element, or `None` if the deque is empty.
    pub fn front(&self) -> Option<&T> {
        self.0.front()
    }

    /// Provides a mutable reference to the front element, or `None` if the deque is empty.
    pub fn front_mut(&mut self) -> Option<&mut T> {
        self.0.front_mut()
    }

    /// Provides a reference to the back element, or `None` if the deque is empty.
    pub fn back(&self) -> Option<&T> {
        self.0.back()
    }

    /// Provides a mutable reference to the back element, or `None` if the deque is empty.
    pub fn back_mut(&mut self) -> Option<&mut T> {
        self.0.back_mut()
    }

    /// Prepends an element to the deque.
    pub fn push_front(&mut self, value: T) {
        self.0.push_front(value)
    }

    /// Appends an element to the back of the deque.
    pub fn push_back(&mut self, value: T) {
        self.0.push_back(value)
    }

    /// Removes the first element and returns it, or `None` if the deque is empty.
    pub fn pop_front(&mut self) -> Option<T> {
        self.0.pop_front()
    }

    /// Removes the last element and returns it, or `None` if the deque is empty.
    pub fn pop_back(&mut self) -> Option<T> {
        self.0.pop_back()
    }

    /// Returns a reference to the element at the given index, or `None` if the index is out of
    /// bounds.
    pub fn get(&self, index: usize) -> Option<&T> {
        self.0.get(index)
    }

    /// Returns a mutable reference to the element at the given index, or `None` if the index is out
    /// of bounds.
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.0.get_mut(index)
    }

    /// Inserts an element at the given index.
    ///
    /// # Panics
    ///
    /// Panics if `index` is greater than the deque's length.
    pub fn insert(&mut self, index: usize, value: T) {
        self.0.insert(index, value)
    }

    /// Removes and returns the element at the given index.
    ///
    /// Returns `None` if `index` is out of bounds.
    pub fn remove(&mut self, index: usize) -> Option<T> {
        self.0.remove(index)
    }

    /// Moves all the elements of `other` into self, leaving `other` empty.
    pub fn append(&mut self, other: &mut Self) {
        self.0.append(&mut other.0)
    }

    /// Splits the deque into two at the given index.
    ///
    /// Returns a newly allocated deque containing the elements in the range `[at, len)`.
    /// After the call, the original deque will be left containing the elements `[0, at)`.
    ///
    /// # Panics
    ///
    /// Panics if `at` is greater than the deque's length.
    pub fn split_off(&mut self, at: usize) -> Self {
        Self(self.0.split_off(at))
    }

    /// Returns an iterator over the elements of the deque.
    pub fn iter(&self) -> IterImpl<'_, T> {
        self.0.iter()
    }

    /// Returns a mutable iterator over the elements of the deque.
    pub fn iter_mut(&mut self) -> IterMutImpl<'_, T> {
        self.0.iter_mut()
    }

    /// Retains only the elements specified by the predicate.
    pub fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&T) -> bool,
    {
        self.0.retain(f)
    }

    /// Retains only the elements specified by the predicate, passing a mutable reference to it.
    pub fn retain_mut<F>(&mut self, f: F)
    where
        F: FnMut(&mut T) -> bool,
    {
        self.0.retain_mut(f)
    }

    /// Extends the deque with the contents of an iterator.
    pub fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.0.extend(iter)
    }
}

impl<T> IntoIterator for Deque<T> {
    type Item = T;
    type IntoIter = IntoIterImpl<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T> Index<usize> for Deque<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl<T> IndexMut<usize> for Deque<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}

impl<T: PartialEq> PartialEq for Deque<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T: Eq> Eq for Deque<T> {}

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
