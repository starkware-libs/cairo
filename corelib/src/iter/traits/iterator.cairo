/// An iterator over a collection of values.
pub trait Iterator<T> {
    /// The type of the elements being iterated over.
    type Item;

    /// Advance the iterator and return the next value.
    fn next(ref self: T) -> Option<Self::Item>;
}
