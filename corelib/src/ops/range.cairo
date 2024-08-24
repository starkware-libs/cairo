use core::ops::AddAssign;
use core::iter::Iterator;
use core::num::traits::One;

pub trait RangeOp<T> {
    type RangeType;

    /// Handles the `..` operator. Returns the value of the expression `lower_bound..upper_bound`.
    fn range(lower_bound: T, upper_bound: T) -> Self::RangeType;
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
