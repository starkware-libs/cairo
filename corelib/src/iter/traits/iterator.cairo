use core::traits::RangeOp;
use core::ops::AddAssign;
use core::num::traits::One;

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

/// Represents the range [lower_bound, upper_bound).
#[derive(Clone, Drop)]
pub struct Range<T> {
    lower_bound: T,
    upper_bound: T,
}

pub impl RangeOpImpl<
    T, +One<T>, +AddAssign<T, T>, +Copy<T>, +Drop<T>, +PartialEq<T>, +PartialOrd<T>
> of RangeOp<T> {
    type RangeType = Range<T>;

    fn range(lower_bound: T, upper_bound: T) -> Self::RangeType {
        if lower_bound < upper_bound {
            Self::RangeType { lower_bound, upper_bound }
        } else {
            // Invalid range, return an empty range.
            Self::RangeType { lower_bound: upper_bound, upper_bound }
        }
    }
}

pub impl RangeIterator<
    T, impl OneT: One<T>, +AddAssign<T, T>, +Copy<T>, +Drop<T>, +PartialEq<T>,
> of Iterator<Range<T>> {
    type Item = T;

    fn next(ref self: Range<T>) -> Option<T> {
        if self.lower_bound != self.upper_bound {
            let value = self.lower_bound;
            self.lower_bound += OneT::one();
            Option::Some(value)
        } else {
            Option::None
        }
    }
}
