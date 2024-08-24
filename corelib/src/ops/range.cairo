use core::iter::Iterator;
use core::num::traits::One;
use core::traits::Add;

/// Handles the range operator (`..`).
pub trait RangeOp<T> {
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

impl RangeOpImpl<
    T, +One<T>, +Add<T>, +Copy<T>, +Drop<T>, +PartialEq<T>, +PartialOrd<T>
> of RangeOp<T> {
    type RangeType = Range<T>;

    fn range(start: T, end: T) -> Self::RangeType {
        if start < end {
            Self::RangeType { start, end }
        } else {
            // Invalid range, return an empty range.
            Self::RangeType { start: end, end }
        }
    }
}

impl RangeIterator<
    T, impl OneT: One<T>, +Add<T>, +Copy<T>, +Drop<T>, +PartialEq<T>,
> of Iterator<Range<T>> {
    type Item = T;

    fn next(ref self: Range<T>) -> Option<T> {
        if self.start != self.end {
            let value = self.start;
            self.start = value + OneT::one();
            Option::Some(value)
        } else {
            Option::None
        }
    }
}
