use crate::iter::adapters::{Enumerate, enumerated_iterator};

/// A trait for dealing with iterators.
///
/// This is the main iterator trait. For more about the concept of iterators
/// generally, please see the [module-level documentation]. In particular, you
/// may want to know how to [implement `Iterator`][impl].
///
/// [module-level documentation]: crate::iter
/// [impl]: crate::iter#implementing-iterator
pub trait Iterator<T> {
    /// The type of the elements being iterated over.
    type Item;

    /// Advance the iterator and return the next value.
    fn next(ref self: T) -> Option<Self::Item>;

    /// Creates an iterator which gives the current iteration count as well as
    /// the next value.
    ///
    /// The iterator returned yields pairs `(i, val)`, where `i` is the
    /// current index of iteration and `val` is the value returned by the
    /// iterator.
    ///
    /// `enumerate()` keeps its count as a [`usize`].
    ///
    /// # Overflow Behavior
    ///
    /// The method does no guarding against overflows, so enumerating more than
    /// [`Bounded::<usize>::MAX`] elements either produces the wrong result or panics.
    ///
    /// [`Bounded`]: core::num::traits::Bounded
    ///
    /// # Panics
    ///
    /// The returned iterator might panic if the to-be-returned index would
    /// overflow a [`usize`].
    ///
    /// # Examples
    ///
    /// ```
    /// let mut iter = array!['a', 'b', 'c'].into_iter().enumerate();
    ///
    /// assert_eq!(iter.next(), Option::Some((0, 'a')));
    /// assert_eq!(iter.next(), Option::Some((1, 'b')));
    /// assert_eq!(iter.next(), Option::Some((2, 'c')));
    /// assert_eq!(iter.next(), Option::None);
    /// ```
    #[inline]
    fn enumerate(self: T) -> Enumerate<T> {
        enumerated_iterator(self)
    }
}
