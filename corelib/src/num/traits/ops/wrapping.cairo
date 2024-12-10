//! Arithmetic operations with overflow and underflow wrapping.
//!
//! This module provides traits for performing arithmetic operations that wrap around at the
//! boundary of the type in case of overflow or underflow. This is particularly useful when you want
//! to:
//! - Perform arithmetic operations without panicking on overflow/underflow
//! - Implement modular arithmetic
//! - Handle cases where overflow is expected and desired
//!
//! # Examples
//!
//! ```
//! use core::num::traits::{WrappingAdd, WrappingSub, WrappingMul};
//!
//! // Addition wrapping
//! let a: u8 = 255;
//! assert!(a.wrapping_add(1) == 0);
//!
//! // Subtraction wrapping
//! let b: u8 = 0;
//! assert!(b.wrapping_sub(1) == 255);
//!
//! // Multiplication wrapping
//! let c: u8 = 200;
//! assert!(c.wrapping_mul(2) == 144); // (200 * 2) % 256 = 144
//! ```

/// Performs addition that wraps around on overflow.
///
/// # Examples
///
/// ```
/// use core::num::traits::WrappingAdd;
///
/// let result = 255_u8.wrapping_add(1);
/// assert!(result == 0);
///
/// let result = 100_u8.wrapping_add(200);
/// assert!(result == 44); // (100 + 200) % 256 = 44
/// ```
pub trait WrappingAdd<T> {
    /// Wrapping (modular) addition. Computes `self + other`, wrapping around at the boundary of the
    /// type.
    fn wrapping_add(self: T, v: T) -> T;
}

/// Performs subtraction that wraps around on overflow.
///
/// # Examples
///
/// ```
/// use core::num::traits::WrappingSub;
///
/// let result = 0_u8.wrapping_sub(1);
/// assert!(result == 255);
///
/// let result = 100_u8.wrapping_sub(150);
/// assert!(result == 206);
/// ```
pub trait WrappingSub<T> {
    /// Wrapping (modular) subtraction. Computes `self - other`, wrapping around at the boundary of
    /// the type.
    fn wrapping_sub(self: T, v: T) -> T;
}

/// Performs multiplication that wraps around on overflow.
///
/// # Examples
///
/// ```
/// use core::num::traits::WrappingMul;
///
/// let result = 10_u8.wrapping_mul(30);
/// assert!(result == 44); // (10 * 30) % 256 = 44
///
/// let result = 200_u8.wrapping_mul(2);
/// assert!(result == 144); // (200 * 2) % 256 = 144
/// ```
pub trait WrappingMul<T> {
    /// Wrapping (modular) multiplication. Computes `self * other`, wrapping around at the boundary
    /// of the type.
    fn wrapping_mul(self: T, v: T) -> T;
}

pub(crate) mod overflow_based {
    pub(crate) impl TWrappingAdd<
        T, +crate::num::traits::OverflowingAdd<T>,
    > of crate::num::traits::WrappingAdd<T> {
        fn wrapping_add(self: T, v: T) -> T {
            let (result, _) = self.overflowing_add(v);
            result
        }
    }

    pub(crate) impl TWrappingSub<
        T, +crate::num::traits::OverflowingSub<T>,
    > of crate::num::traits::WrappingSub<T> {
        fn wrapping_sub(self: T, v: T) -> T {
            let (result, _) = self.overflowing_sub(v);
            result
        }
    }

    pub(crate) impl TWrappingMul<
        T, +crate::num::traits::OverflowingMul<T>,
    > of crate::num::traits::WrappingMul<T> {
        fn wrapping_mul(self: T, v: T) -> T {
            let (result, _) = self.overflowing_mul(v);
            result
        }
    }
}
