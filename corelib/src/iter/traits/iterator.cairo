/// An iterator over a collection of values.
pub trait Iterator<T> {
    /// The type of the elements being iterated over.
    type Item;

    /// Advance the iterator and return the next value.
    fn next(ref self: T) -> Option<Self::Item>;
}

/// Turn a collection of values into an iterator.
#[unstable(feature: "collections-into-iter")]
pub trait IntoIterator<T> {
    /// The iterator type that will be created.
    type IntoIter;

    /// Creates an iterator from a collection.
    fn into_iter(ref self: T) -> Self::IntoIter;
}
