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

impl IteratorIntoIterator<T, +Iterator<T>> of IntoIterator<T> {
    type IntoIter = T;
    fn into_iter(self: T) -> T {
        self
    }
}

impl SnapshotFixedSizeArrayIntoIterator<
    T, const SIZE: usize, +Drop<T>, impl ToSpan: core::array::ToSpanTrait<[T; SIZE], T>,
> of IntoIterator<@[T; SIZE]> {
    type IntoIter = crate::array::SpanIter<T>;
    fn into_iter(self: @[T; SIZE]) -> Self::IntoIter {
        ToSpan::span(self).into_iter()
    }
}
