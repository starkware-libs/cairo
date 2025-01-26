//! Safe arithmetic operations with overflow/underflow checking.
//!
//! This module provides traits for performing arithmetic operations with explicit
//! overflow and underflow protection. These operations return `None` when an overflow
//! or underflow occurs, allowing you to handle these cases gracefully without panicking.
//!
//! # Examples
//!
//! ```
//! use core::num::traits::{CheckedAdd, CheckedSub, CheckedMul};
//!
//! // Checked addition
//! let a: u8 = 1;
//! assert!(a.checked_add(2) == Some(3));
//! assert!(a.checked_add(255) == None); // Overflow
//!
//! // Checked subtraction
//! let b: u8 = 1;
//! assert!(b.checked_sub(1) == Some(0));
//! assert!(b.checked_sub(2) == None); // Underflow
//!
//! // Checked multiplication
//! let c: u8 = 10;
//! assert!(c.checked_mul(20) == Some(200));
//! assert!(c.checked_mul(30) == None); // Overflow
//! ```

/// Performs addition that returns `None` instead of wrapping around on
/// overflow.
///
/// # Examples
///
/// ```
/// use core::num::traits::CheckedAdd;
///
/// let result = 1_u8.checked_add(2);
/// assert!(result == Some(3));
///
/// let result = 255_u8.checked_add(1);
/// assert!(result == None); // Overflow
/// ```
pub trait CheckedAdd<T> {
    /// Adds two numbers, checking for overflow. If overflow happens, `None` is
    /// returned.
    fn checked_add(self: T, v: T) -> Option<T>;
}

/// Performs subtraction that returns `None` instead of wrapping around on underflow.
///
/// # Examples
///
/// ```
/// use core::num::traits::CheckedSub;
///
/// let result = 1_u8.checked_sub(1);
/// assert!(result == Some(0));
///
/// let result = 1_u8.checked_sub(2);
/// assert!(result == None); // Underflow
/// ```
pub trait CheckedSub<T> {
    /// Subtracts two numbers, checking for underflow. If underflow happens,
    /// `None` is returned.
    fn checked_sub(self: T, v: T) -> Option<T>;
}

/// Performs multiplication that returns `None` instead of wrapping around on underflow or
/// overflow.
///
/// # Examples
///
/// ```
/// use core::num::traits::CheckedMul;
///
/// let result = 10_u8.checked_mul(20);
/// assert!(result == Some(200));
///
/// let result = 10_u8.checked_mul(30);
/// assert!(result == None); // Overflow
/// ```
pub trait CheckedMul<T> {
    /// Multiplies two numbers, checking for underflow or overflow. If underflow
    /// or overflow happens, `None` is returned.
    fn checked_mul(self: T, v: T) -> Option<T>;
}

pub(crate) mod overflow_based {
    pub(crate) impl TCheckedAdd<
        T, +Drop<T>, +crate::num::traits::OverflowingAdd<T>,
    > of crate::num::traits::CheckedAdd<T> {
        fn checked_add(self: T, v: T) -> Option<T> {
            let (result, overflow) = self.overflowing_add(v);
            match overflow {
                true => None,
                false => Some(result),
            }
        }
    }

    pub(crate) impl TCheckedSub<
        T, +Drop<T>, +crate::num::traits::OverflowingSub<T>,
    > of crate::num::traits::CheckedSub<T> {
        fn checked_sub(self: T, v: T) -> Option<T> {
            let (result, overflow) = self.overflowing_sub(v);
            match overflow {
                true => None,
                false => Some(result),
            }
        }
    }

    pub(crate) impl TCheckedMul<
        T, +Drop<T>, +crate::num::traits::OverflowingMul<T>,
    > of crate::num::traits::CheckedMul<T> {
        fn checked_mul(self: T, v: T) -> Option<T> {
            let (result, overflow) = self.overflowing_mul(v);
            match overflow {
                true => None,
                false => Some(result),
            }
        }
    }
}
