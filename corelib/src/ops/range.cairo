//! Range and iteration utilities.
//!
//! This module provides functionality for creating and iterating over ranges of values.
//! A range represents an interval of values from a start point to an end point.
//!
//! # Range Operator Forms
//!
//! There is currently only a single range operator form: `start..end`, representing a range from
//! `start` (inclusive) to `end` (exclusive).

use core::iter::{IntoIterator, Iterator};
use core::num::traits::One;
use core::traits::Add;

/// A (half-open) range bounded inclusively below and exclusively above
/// (`start..end`).
///
/// The range `start..end` contains all values with `start <= x < end`.
/// It is empty if `start >= end`.
///
/// # Examples
///
/// The `start..end` syntax is a `Range`:
///
/// ```
/// assert!((3..5) == core::ops::Range { start: 3, end: 5 });
///
/// let mut sum = 0;
/// for i in 3..6 {
///     sum += i;
/// }
/// assert!(sum == 3 + 4 + 5);
/// ```
#[derive(Clone, Drop, PartialEq)]
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
    fn fmt(self: @Range<T>, ref f: crate::fmt::Formatter) -> Result<(), crate::fmt::Error> {
        self.start.fmt(ref f)?;
        write!(f, "..")?;
        self.end.fmt(ref f)?;
        Result::Ok(())
    }
}

/// Handles the range binary operator (`..`).
/// Used by the compiler to create a `Range` from the given `start` (inclusive) and `end`
/// (exclusive) values.
#[generate_trait]
pub impl RangeOpImpl<T> of RangeOp<T> {
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

/// An endpoint of a range of keys.
///
/// # Examples
///
/// `Bound`s are range endpoints:
///
/// ```
/// use core::ops::{Bounds, RangeBounds};
///
/// assert_eq!((1..12).start_bound(), Bound::Included(@1));
/// assert_eq!((1..12).end_bound(), Bound::Excluded(@12));
/// ```
#[derive(Clone, Drop, Debug, PartialEq)]
pub enum Bound<T> {
    /// An inclusive bound.
    Included: T,
    /// An exclusive bound.
    Excluded: T,
    /// An infinite endpoint. Indicates that there is no bound in this direction.
    Unbounded,
}


/// `RangeBounds` is implemented by Cairo's built-in range types, produced
/// by range syntax like `a..b`
pub trait RangeBounds<T> {
    type Item;
    //BUG: adding a trait bound at this level crashes the compiler
    // impl ItemPartialOrd: PartialOrd<@Self::Item>;

    /// Start index bound.
    ///
    /// Returns the start value as a `Bound`.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::ops::{Bounds, RangeBounds};
    ///
    /// assert!( (3_u8..5).start_bound() == Bound::Included(@3));
    /// ```
    fn start_bound(self: @T) -> Bound<@Self::Item>;

    /// End index bound.
    ///
    /// Returns the end value as a `Bound`.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!( (3_u8..5).end_bound() == Bound::Excluded(@5));
    /// ```
    fn end_bound(self: @T) -> Bound<@Self::Item>;

    /// Returns `true` if `item` is contained in the range.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::ops::RangeBounds;
    ///
    /// assert!( (3_u8..5).contains(@4));
    /// assert!(!(3_u8..5).contains(@2));
    /// ```
    fn contains(self: @T, item: @Self::Item) -> bool;
}

impl RangeBoundsRangeImpl<T, +Destruct<T>, +PartialOrd<@T>> of RangeBounds<Range<T>> {
    type Item = T;

    #[inline]
    fn start_bound(self: @Range<T>) -> Bound<@T> {
        Bound::Included(self.start)
    }

    #[inline]
    fn end_bound(self: @Range<T>) -> Bound<@T> {
        Bound::Excluded(self.end)
    }

    //BUG: adding this as a default trait impl crashes the compiler
    #[inline]
    fn contains(self: @Range<T>, item: @T) -> bool {
        (match Self::start_bound(self) {
            Bound::Included(start) => start <= item,
            Bound::Excluded(start) => start < item,
            Bound::Unbounded => true,
        })
            && (match Self::end_bound(self) {
                Bound::Included(end) => item <= end,
                Bound::Excluded(end) => item < end,
                Bound::Unbounded => true,
            })
    }
}
