//! Utilities for comparing and ordering values.
//! This module contains functions that rely on the `PartialOrd` trait for comparing values.
//!
//! ### Examples
//!
//! ```
//! use core::cmp::{min, max, minmax};
//!
//! let a = 10;
//! let b = 20;
//! assert!(min(a, b) == 10);
//! assert!(max(a, b) == 20);
//!
//! let (min_value, max_value) = minmax(a, b);
//! assert!((min_value, max_value) == (10, 20));
//! ```

/// Takes two comparable values `a` and `b` and returns
/// the smallest of the two values.
///
/// # Examples
///
/// ```
/// use core::cmp::min;
///
/// let a = 0;
/// let b = 1;
/// let min = min(a,b);
/// assert!(min == 0);
/// ```
#[must_use]
pub fn min<T, +PartialOrd<T>, +Drop<T>, +Copy<T>>(a: T, b: T) -> T {
    if a > b {
        b
    } else {
        a
    }
}

/// Takes two comparable values `a` and `b` and returns
/// the greatest of the two values.
///
/// # Examples
///
/// ```
/// use core::cmp::max;
///
/// let a = 0;
/// let b = 1;
/// let max = max(a,b);
/// assert!(min == 1);
/// ```
#[must_use]
pub fn max<T, +PartialOrd<T>, +Drop<T>, +Copy<T>>(a: T, b: T) -> T {
    if a > b {
        a
    } else {
        b
    }
}

/// Takes two comparable values `a` and `b` and returns
/// a tuple with the smallest value and the greatest value.
///
/// # Examples
///
/// ```
/// use core::cmp::minmax;
///
/// let a = 0;
/// let b = 1;
/// let minmax_tuple = minmax(a,b);
/// assert!(minmax_tuple == (0, 1));
/// ```
#[must_use]
pub fn minmax<T, +PartialOrd<T>, +Drop<T>, +Copy<T>>(a: T, b: T) -> (T, T) {
    if a > b {
        (b, a)
    } else {
        (a, b)
    }
}
