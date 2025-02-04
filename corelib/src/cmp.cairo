//! Utilities for comparing and ordering values.
//! This module contains functions that rely on the `PartialOrd` trait for comparing values.
//!
//! # Examples
//!
//! ```
//! use core::cmp::{min, max, minmax};
//!
//! assert!(min(10, 20) == 10);
//! assert!(max(10, 20) == 20);
//!
//! assert!(minmax(20, 10) == (10, 20));
//! assert!(minmax(10, 20) == (10, 20));
//! ```

/// Takes two comparable values `a` and `b` and returns
/// the smaller of the two values.
///
/// # Examples
///
/// ```
/// use core::cmp::min;
///
/// assert!(min(0, 1) == 0);
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
/// the greater of the two values.
///
/// # Examples
///
/// ```
/// use core::cmp::max;
///
/// assert!(max(0, 1) == 1);
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
/// a tuple with the smaller value and the greater value.
///
/// # Examples
///
/// ```
/// use core::cmp::minmax;
///
/// assert!(minmax(0, 1) == (0, 1));
/// assert!(minmax(1, 0) == (0, 1));
/// ```
#[must_use]
pub fn minmax<T, +PartialOrd<T>, +Drop<T>, +Copy<T>>(a: T, b: T) -> (T, T) {
    if a > b {
        (b, a)
    } else {
        (a, b)
    }
}
