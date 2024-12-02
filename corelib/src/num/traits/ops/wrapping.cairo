//! Arithmetic operations with overflow and underflow wrapping.
//!
//! This module provides traits for performing arithmetic operations that wrap the result in case
//! of overflow or underflow.

/// A trait for performing addition that wraps around on overflow.
pub trait WrappingAdd<T> {
    /// Wrapping (modular) addition. Computes `self + other`, wrapping around at the boundary of the
    /// type.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(255_u8.wrapping_add(1) == 0);
    /// ```
    fn wrapping_add(self: T, v: T) -> T;
}

/// A trait for performing subtraction that wraps around on overflow.
pub trait WrappingSub<T> {
    /// Wrapping (modular) subtraction. Computes `self - other`, wrapping around at the boundary of
    /// the type.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(0_u8.wrapping_sub(1) == 255);
    /// ```
    fn wrapping_sub(self: T, v: T) -> T;
}

/// A trait for performing multiplication that wraps around on overflow.
pub trait WrappingMul<T> {
    /// Wrapping (modular) multiplication. Computes `self * other`, wrapping around at the boundary
    /// of the type.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(10_u8.wrapping_mul(30) == 44);
    /// ```
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
