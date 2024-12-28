use crate::iter::adapters::{Map, MapTrait};

/// An iterator over a collection of values.
pub trait Iterator<T> {
    /// The type of the elements being iterated over.
    type Item;

    /// Advance the iterator and return the next value.
    fn next(ref self: T) -> Option<Self::Item>;

    #[inline]
    fn map<
        B,
        F,
        impl TIter: Self,
        +core::ops::FnOnce<F, (TIter::Item,)>[Output: B],
        +Drop<T>,
        +Drop<F>,
        +Copy<F>,
    >(
        self: T, f: F,
    ) -> Map<T, F> {
        MapTrait::new(self, f)
    }
}
