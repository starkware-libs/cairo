use core::iter::{IntoIterator, Iterator};
use core::num::traits::One;
use core::traits::Add;

/// Handles the range operator (`..`).
trait RangeOp<T> {
    /// The result type of the expression `start..end`.
    type RangeType;

    /// Handles the `..` operator. Returns the value of the expression `start..end`.
    fn range(start: T, end: T) -> Self::RangeType;
}

/// Represents the range [start, end).
#[derive(Clone, Drop)]
pub struct Range<T> {
    /// The lower bound of the range (inclusive).
    start: T,
    /// The upper bound of the range (exclusive).
    end: T,
}

/// Represents an iterator located at `cur`, whose end is `end` (`cur <= end`).
#[derive(Clone, Drop)]
pub struct RangeIterator<T> {
    /// The current value of the iterator.
    cur: T,
    /// The upper bound of the range (exclusive).
    end: T,
}

impl RangeOpImpl<
    T, +One<T>, +Add<T>, +Copy<T>, +Drop<T>, +PartialEq<T>, +PartialOrd<T>
> of RangeOp<T> {
    type RangeType = Range<T>;

    fn range(start: T, end: T) -> Self::RangeType {
        Self::RangeType { start: start, end: end }
    }
}

impl RangeIteratorImpl<
    T, impl OneT: One<T>, +Add<T>, +Copy<T>, +Drop<T>, +PartialEq<T>,
> of Iterator<RangeIterator<T>> {
    type Item = T;

    fn next(ref self: RangeIterator<T>) -> Option<T> {
        if self.cur != self.end {
            let value = self.cur;
            self.cur = value + OneT::one();
            Option::Some(value)
        } else {
            Option::None
        }
    }
}

impl RangeIntoIterator<
    T, +One<T>, +Add<T>, +Copy<T>, +Drop<T>, +PartialEq<T>, +PartialOrd<T>
> of IntoIterator<Range<T>> {
    type IntoIter = RangeIterator<T>;
    impl Iterator = RangeIteratorImpl<T>;

    fn into_iter(self: Range<T>) -> Self::IntoIter {
        let start = self.start;
        let end = self.end;
        if start < end {
            Self::IntoIter { cur: start, end }
        } else {
            // Invalid range, return an empty range.
            Self::IntoIter { cur: end, end }
        }
    }
}
