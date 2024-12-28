use core::ops::Range;
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
}

/// Turn a collection of values into an iterator.
pub trait IntoIterator<T> {
    /// The iterator type that will be created.
    type IntoIter;
    impl Iterator: Iterator<Self::IntoIter>;
    /// Creates an iterator from a collection.
    fn into_iter(self: T) -> Self::IntoIter;
}

/// Turn a collection of values into an iterator over a specific range.
pub trait IntoIterRange<T, I> {
    type IntoIter;
    impl Iterator: Iterator<Self::IntoIter>;
    /// Creates an iterator over a range from a collection.
    fn into_iter_range(self: T, range: Range<I>) -> Self::IntoIter;
    /// Creates an iterator over the full range of a collection.
    fn into_iter_full_range(self: T) -> Self::IntoIter;
}

impl IteratorIntoIterator<T, +Iterator<T>> of IntoIterator<T> {
    type IntoIter = T;
    fn into_iter(self: T) -> T {
        self
    }
}
