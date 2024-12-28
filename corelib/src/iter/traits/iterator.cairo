use core::ops::Range;

/// An iterator over a collection of values.
pub trait Iterator<T> {
    /// The type of the elements being iterated over.
    type Item;

    /// Advance the iterator and return the next value.
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
