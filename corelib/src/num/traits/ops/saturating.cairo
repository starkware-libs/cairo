//! Saturating arithmetic operations for numeric types.
//!
//! This module provides traits and implementations for arithmetic operations
//! that saturate at the numeric type's boundaries instead of overflowing.

/// Performs addition that saturates at the numeric bounds instead of overflowing.
///
/// # Examples
///
/// ```
/// use core::num::traits::SaturatingAdd;
///
/// assert!(255_u8.saturating_add(1_u8) == 255);
/// ```
pub trait SaturatingAdd<T> {
    /// Saturating addition. Computes `self + other`, saturating at the relevant high or low
    /// boundary of the type.
    fn saturating_add(self: T, other: T) -> T;
}

/// Performs subtraction that saturates at the numeric bounds instead of overflowing.
///
/// # Examples
///
/// ```
/// use core::num::traits::SaturatingSub;
///
/// assert!(1_u8.saturating_sub(2_u8) == 0);
/// ```
pub trait SaturatingSub<T> {
    /// Saturating subtraction. Computes `self - other`, saturating at the relevant high or low
    /// boundary of the type.
    fn saturating_sub(self: T, other: T) -> T;
}

/// Performs multiplication that saturates at the numeric bounds instead of overflowing.
///
/// # Examples
///
/// ```
/// use core::num::traits::SaturatingMul;
///
/// assert!(100_u8.saturating_mul(3_u8) == 255);
/// ```
pub trait SaturatingMul<T> {
    /// Saturating multiplication. Computes `self * other`, saturating at the relevant high or low
    /// boundary of the type.
    fn saturating_mul(self: T, other: T) -> T;
}

pub(crate) mod overflow_based {
    pub(crate) impl TSaturatingAdd<
        T, +Drop<T>, +crate::num::traits::OverflowingAdd<T>, +crate::num::traits::Bounded<T>,
    > of crate::num::traits::SaturatingAdd<T> {
        fn saturating_add(self: T, other: T) -> T {
            let (result, overflow) = self.overflowing_add(other);
            match overflow {
                true => crate::num::traits::Bounded::MAX,
                false => result,
            }
        }
    }

    pub(crate) impl TSaturatingSub<
        T, +Drop<T>, +crate::num::traits::OverflowingSub<T>, +crate::num::traits::Bounded<T>,
    > of crate::num::traits::SaturatingSub<T> {
        fn saturating_sub(self: T, other: T) -> T {
            let (result, overflow) = self.overflowing_sub(other);
            match overflow {
                true => crate::num::traits::Bounded::MIN,
                false => result,
            }
        }
    }

    pub(crate) impl TSaturatingMul<
        T, +Drop<T>, +crate::num::traits::OverflowingMul<T>, +crate::num::traits::Bounded<T>,
    > of crate::num::traits::SaturatingMul<T> {
        fn saturating_mul(self: T, other: T) -> T {
            let (result, overflow) = self.overflowing_mul(other);
            match overflow {
                true => crate::num::traits::Bounded::MAX,
                false => result,
            }
        }
    }
}
