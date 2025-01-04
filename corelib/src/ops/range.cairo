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

#[generate_trait]
pub impl RangeImpl<T, +Copy<T>, +Drop<T>, +PartialOrd<T>> of RangeTrait<T> {
    /// Returns `true` if the range contains no items.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(!(3_u8..5_u8).is_empty());
    /// assert!( (3_u8..3_u8).is_empty());
    /// assert!( (3_u8..2_u8).is_empty());
    /// ```
    #[inline]
    fn is_empty(self: @Range<T>) -> bool {
        !(self.start < self.end)
    }
}

impl RangeDebug<T, impl TDebug: crate::fmt::Debug<T>> of crate::fmt::Debug<Range<T>> {
    /// Formats a `Range` type, allowing to print `Range` instances for debugging purposes.
    ///
    /// # Examples
    ///
    /// ```
    /// println!("{:?}", 1..5); // Result will be `1..5`
    /// ```
    fn fmt(self: @Range<T>, ref f: crate::fmt::Formatter) -> Result<(), crate::fmt::Error> {
        self.start.fmt(ref f)?;
        write!(f, "..")?;
        self.end.fmt(ref f)?;
        Result::Ok(())
    }
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
    T,
    +One<T>,
    +Add<T>,
    +Copy<T>,
    +Drop<T>,
    +PartialEq<T>,
    +PartialOrd<T>,
    -SierraIntRangeSupport<T>,
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

/// Represents the range [start, end].
#[derive(Clone, Drop, PartialEq)]
pub struct RangeInclusive<T> {
    /// The lower bound of the range (inclusive).
    pub start: T,
    /// The upper bound of the range (inclusive).
    pub end: T,
}

#[derive(Clone, Drop)]
pub struct RangeInclusiveIterator<T> {
    /// The current value of the iterator.
    pub(crate) cur: T,
    /// The upper bound of the range (inclusive).
    pub(crate) end: T,
    // This field is:
    //  - `false` upon construction
    //  - `false` when iteration has yielded an element and the iterator is not exhausted
    //  - `true` when iteration has been used to exhaust the iterator
    //
    // This is required to differentiate bewteen the last element and the end of the range.
    pub(crate) exhausted: bool,
}

/// Handles the range inclusive operator (`..=`).
#[generate_trait]
pub impl RangeInclusiveOpImpl<T> of RangeInclusiveOp<T> {
    /// Handles the `..=` operator. Returns the value of the expression `start..=end`.
    fn range_inclusive(start: T, end: T) -> RangeInclusive<T> {
        RangeInclusive { start, end }
    }
}

impl RangeInclusiveDebug<T, impl TDebug: crate::fmt::Debug<T>> of crate::fmt::Debug<RangeInclusive<T>> {
    fn fmt(self: @RangeInclusive<T>, ref f: crate::fmt::Formatter) -> Result<(), crate::fmt::Error> {
        self.start.fmt(ref f)?;
        write!(f, "..=")?;
        self.end.fmt(ref f)?;
        Result::Ok(())
    }
}

impl RangeInclusiveIteratorImpl<
    T, +One<T>, +Add<T>, +Copy<T>, +Drop<T>, +PartialEq<T>, +PartialOrd<T>,
> of Iterator<RangeInclusiveIterator<T>> {
    type Item = T;

    fn next(ref self: RangeInclusiveIterator<T>) -> Option<T> {
        if self.exhausted {
            return Option::None;
        }

        let current = self.cur;

        // If this is the last element, mark as exhausted for next iteration
        if current == self.end {
            self.exhausted = true;
            return Option::Some(current);
        }

        // We know current < self.end here, because the iterator is not exhausted
        self.cur = current + One::one();
        Option::Some(current)
    }
}

pub impl RangeInclusiveIntoIterator<
    T,
    +One<T>,
    +Add<T>,
    +Copy<T>,
    +Drop<T>,
    +PartialEq<T>,
    +PartialOrd<T>,
    -SierraIntRangeSupport<T>,
> of IntoIterator<RangeInclusive<T>> {
    type IntoIter = RangeInclusiveIterator<T>;

    fn into_iter(self: RangeInclusive<T>) -> Self::IntoIter {
        let exhausted = self.start > self.end;
        Self::IntoIter { cur: self.start, end: self.end, exhausted }
    }
}


// Sierra optimization.

mod internal {
    use core::internal::OptionRev;
    use core::iter::Iterator;

    #[derive(Copy, Drop)]
    pub extern type IntRange<T>;
    pub extern fn int_range_try_new<T>(
        x: T, y: T,
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
    T, +Copy<T>, +Drop<T>, +SierraIntRangeSupport<T>,
> of IntoIterator<Range<T>> {
    type IntoIter = internal::IntRange<T>;

    fn into_iter(self: Range<T>) -> Self::IntoIter {
        match internal::int_range_try_new(self.start, self.end) {
            Result::Ok(range) => range,
            Result::Err(range) => range,
        }
    }
}
