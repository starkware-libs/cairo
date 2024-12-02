//! Arithmetic operations with overflow detection.
//!
//! This module provides traits for performing arithmetic operations that explicitly
//! track potential numeric overflow conditions.

/// Performs addition with a flag for overflow.
///
/// # Examples
///
/// ```
/// use core::num::traits::OverflowingAdd;
///
/// let (result, is_overflow) = 1_u8.overflowing_add(255_u8);
/// assert!(result == 0);
/// assert!(is_overflow);
/// ```
pub trait OverflowingAdd<T> {
    /// Returns a tuple of the sum along with a boolean indicating whether an arithmetic overflow
    /// would occur.
    /// If an overflow would have occurred then the wrapped value is returned.
    fn overflowing_add(self: T, v: T) -> (T, bool);
}

/// Performs subtraction with a flag for overflow.
///
/// # Examples
///
/// ```
/// use core::num::traits::OverflowingSub;
///
/// let (result, is_underflow) = 1_u8.overflowing_sub(2_u8);
/// assert!(result == 255);
/// assert!(is_underflow);
/// ```
pub trait OverflowingSub<T> {
    /// Returns a tuple of the difference along with a boolean indicating whether an arithmetic
    /// overflow would occur.
    /// If an overflow would have occurred then the wrapped value is returned.
    fn overflowing_sub(self: T, v: T) -> (T, bool);
}

/// Performs multiplication with a flag for overflow.
///
/// # Examples
///
/// ```
/// use core::num::traits::OverflowingMul;
///
/// let (result, is_overflow) = 1_u8.overflowing_mul(2_u8);
/// assert!(result == 2);
/// assert!(!is_overflow);
/// ```
pub trait OverflowingMul<T> {
    /// Returns a tuple of the product along with a boolean indicating whether an arithmetic
    /// overflow would occur.
    /// If an overflow would have occurred then the wrapped value is returned.
    fn overflowing_mul(self: T, v: T) -> (T, bool);
}
