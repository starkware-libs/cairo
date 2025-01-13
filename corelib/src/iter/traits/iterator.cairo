use core::iter::adapters::{Chain, chained_iterator};
use core::metaprogramming::TypeEqual;

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

    /// Advances the iterator and returns the next value.
    ///
    /// Returns [`None`] when iteration is finished. Individual iterator
    /// implementations may choose to resume iteration, and so calling `next()`
    /// again may or may not eventually start returning [`Some(Item)`] again at some
    /// point.
    ///
    /// [`Some(Item)`]: Option::Some
    /// [`None`]: Option::None
    ///
    /// # Examples
    ///
    /// ```
    /// let mut iter = [1, 2, 3].span().into_iter();
    ///
    /// // A call to next() returns the next value...
    /// assert_eq!(Option::Some(@1), iter.next());
    /// assert_eq!(Option::Some(@2), iter.next());
    /// assert_eq!(Option::Some(@3), iter.next());
    ///
    /// // ... and then None once it's over.
    /// assert_eq!(Option::None, iter.next());
    ///
    /// // More calls may or may not return `None`. Here, they always will.
    /// assert_eq!(Option::None, iter.next());
    /// assert_eq!(Option::None, iter.next());
    /// ```
    fn next(ref self: T) -> Option<Self::Item>;

    /// Takes two iterators and creates a new iterator over both in sequence.
    ///
    /// `chain()` will return a new iterator which will first iterate over
    /// values from the first iterator and then over values from the second
    /// iterator.
    ///
    /// In other words, it links two iterators together, in a chain. 🔗
    ///
    /// Arguments do not have to be of the same type as long as the underlying iterated
    /// over items are.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use core::ops::Range;
    ///
    /// let a: Array<u8> = array![7, 8, 9];
    /// let b: Range<u8> = 0..5;
    ///
    /// let mut iter = a.into_iter().chain(b.into_iter());
    ///
    /// assert_eq!(iter.next(), Option::Some(7));
    /// assert_eq!(iter.next(), Option::Some(8));
    /// assert_eq!(iter.next(), Option::Some(9));
    /// assert_eq!(iter.next(), Option::Some(0));
    /// assert_eq!(iter.next(), Option::Some(1));
    /// assert_eq!(iter.next(), Option::Some(2));
    /// assert_eq!(iter.next(), Option::Some(3));
    /// assert_eq!(iter.next(), Option::Some(4));
    /// assert_eq!(iter.next(), Option::None);
    /// ```
    ///
    /// Since the argument to `chain()` uses [`IntoIterator`], we can pass
    /// anything that can be converted into an [`Iterator`], not just an
    /// [`Iterator`] itself. For example, arrays implement
    /// [`IntoIterator`], and so can be passed to `chain()` directly:
    ///
    /// ```
    /// let a = array![1, 2, 3];
    /// let b = array![4, 5, 6];
    ///
    /// let mut iter = a.into_iter().chain(b);
    ///
    /// assert_eq!(iter.next(), Option::Some(1));
    /// assert_eq!(iter.next(), Option::Some(2));
    /// assert_eq!(iter.next(), Option::Some(3));
    /// assert_eq!(iter.next(), Option::Some(4));
    /// assert_eq!(iter.next(), Option::Some(5));
    /// assert_eq!(iter.next(), Option::Some(6));
    /// assert_eq!(iter.next(), Option::None);
    /// ```
    fn chain<U, impl IntoIterU: IntoIterator<U>, +TypeEqual<Self::Item, IntoIterU::Iterator::Item>>(
        self: T, other: U,
    ) -> Chain<T, IntoIterU::IntoIter>
    {
        chained_iterator(self, other.into_iter())
    }
}
