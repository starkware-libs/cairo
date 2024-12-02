//! Arithmetic operations with overflow and underflow detection.
//!
//! This module provides traits for performing arithmetic operations that explicitly
//! track potential numeric overflow conditions.

/// A trait for performing addition with a flag for overflow.
pub trait OverflowingAdd<T> {
    /// Returns a tuple of the sum along with a boolean indicating whether an arithmetic overflow
    /// would occur.
    /// If an overflow would have occurred then the wrapped value is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// let (result, is_overflow) = 1_u8.overflowing_add(2_u8);
    /// assert!(result == 3);
    /// assert!(!is_overflow);
    /// ```
    fn overflowing_add(self: T, v: T) -> (T, bool);
}

/// A trait for performing subtraction with a flag for underflow.
pub trait OverflowingSub<T> {
    /// Returns a tuple of the difference along with a boolean indicating whether an arithmetic
    /// underflow would occur.
    /// If an underflow would have occurred then the wrapped value is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// let (result, is_underflow) = 1_u8.overflowing_sub(2_u8);
    /// assert!(result == 255);
    /// assert!(is_underflow);
    /// ```
    fn overflowing_sub(self: T, v: T) -> (T, bool);
}

/// A trait for performing multiplication with a flag for overflow.
pub trait OverflowingMul<T> {
    /// Returns a tuple of the product along with a boolean indicating whether an arithmetic
    /// overflow would occur.
    /// If an overflow would have occurred then the wrapped value is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// let (result, is_overflow) = 1_u8.overflowing_mul(2_u8);
    /// assert!(result == 2);
    /// assert!(!is_overflow);
    /// ```
    fn overflowing_mul(self: T, v: T) -> (T, bool);
}
