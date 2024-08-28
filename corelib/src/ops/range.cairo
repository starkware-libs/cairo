use core::iter::{IntoIterator, Iterator};
use core::num::traits::One;
use core::traits::Add;

/// Represents the range [start, end).
#[derive(Clone, Drop)]
pub struct Range<T> {
    /// The lower bound of the range (inclusive).
    pub start: T,
    /// The upper bound of the range (exclusive).
    pub end: T,
}

/// Handles the range operator (`..`).
#[generate_trait]
pub impl RangeOpImpl<T> of RangeOp<T> {
    /// Handles the `..` operator. Returns the value of the expression `start..end`.
    fn range(start: T, end: T) -> Range<T> {
        Range { start, end }
    }
}

/// Represents an iterator located at `cur`, whose end is `end` (`cur <= end`).
#[derive(Clone, Drop)]
pub struct RangeIterator<T> {
    /// The current value of the iterator.
    cur: T,
    /// The upper bound of the range (exclusive).
    end: T,
}

impl RangeIteratorImpl<
    T, impl OneT: One<T>, +Add<T>, +Copy<T>, +Drop<T>, +PartialEq<T>
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
    T,
    +One<T>,
    +Add<T>,
    +Copy<T>,
    +Drop<T>,
    +PartialEq<T>,
    +PartialOrd<T>,
    -SierraIntRangeSupport<T>
> of IntoIterator<Range<T>> {
    type IntoIter = RangeIterator<T>;

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

// Sierra optimization.

mod internal {
    use core::iter::Iterator;
    use core::internal::OptionRev;

    #[derive(Copy, Drop)]
    pub extern type IntRange<T>;
    pub extern fn int_range_try_new<T>(
        x: T, y: T
    ) -> Result<IntRange<T>, IntRange<T>> implicits(core::RangeCheck) nopanic;
    pub extern fn int_range_pop_front<T>(range: IntRange<T>) -> OptionRev<(IntRange<T>, T)> nopanic;

    impl IntRangeIteratorImpl<T, +Copy<T>, +Drop<T>> of Iterator<IntRange<T>> {
        type Item = T;

        fn next(ref self: IntRange<T>) -> Option<T> {
            match int_range_pop_front(self) {
                OptionRev::None => Option::None,
                OptionRev::Some((new_range, value)) => {
                    self = new_range;
                    Option::Some(value)
                },
            }
        }
    }
}

/// Marker trait to enable using the Sierra libfuncs for integer range iteration (`IntRange`).
trait SierraIntRangeSupport<T>;

impl SierraRangeIntoIterator<
    T, +Copy<T>, +Drop<T>, +SierraIntRangeSupport<T>
> of IntoIterator<Range<T>> {
    type IntoIter = internal::IntRange<T>;

    fn into_iter(self: Range<T>) -> Self::IntoIter {
        match internal::int_range_try_new(self.start, self.end) {
            Result::Ok(range) => range,
            Result::Err(range) => range,
        }
    }
}
