//! Safe arithmetic operations with overflow/underflow checking.
//!
//! This module provides traits for performing arithmetic operations with explicit
//! overflow and underflow protection.

/// A trait for performing addition that returns `None` instead of wrapping around on
/// overflow.
pub trait CheckedAdd<T> {
    /// Adds two numbers, checking for overflow. If overflow happens, `None` is
    /// returned.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(1_u8.checked_add(2_u8) == Option::Some(3));
    /// ```
    fn checked_add(self: T, v: T) -> Option<T>;
}

/// A trait for performing subtraction that returns `None` instead of wrapping around on underflow.
pub trait CheckedSub<T> {
    /// Subtracts two numbers, checking for underflow. If underflow happens,
    /// `None` is returned.
    ///
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(1_u8.checked_sub(2_u8) == Option::None);
    /// ```
    fn checked_sub(self: T, v: T) -> Option<T>;
}

/// A trait for performing multiplication that returns `None` instead of wrapping around on
/// underflow or overflow.
pub trait CheckedMul<T> {
    /// Multiplies two numbers, checking for underflow or overflow. If underflow
    /// or overflow happens, `None` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(1_u8.checked_mul(2_u8) == Option::Some(2));
    /// ````
    fn checked_mul(self: T, v: T) -> Option<T>;
}

pub(crate) mod overflow_based {
    pub(crate) impl TCheckedAdd<
        T, +Drop<T>, +crate::num::traits::OverflowingAdd<T>,
    > of crate::num::traits::CheckedAdd<T> {
        fn checked_add(self: T, v: T) -> Option<T> {
            let (result, overflow) = self.overflowing_add(v);
            match overflow {
                true => Option::None,
                false => Option::Some(result),
            }
        }
    }

    pub(crate) impl TCheckedSub<
        T, +Drop<T>, +crate::num::traits::OverflowingSub<T>,
    > of crate::num::traits::CheckedSub<T> {
        fn checked_sub(self: T, v: T) -> Option<T> {
            let (result, overflow) = self.overflowing_sub(v);
            match overflow {
                true => Option::None,
                false => Option::Some(result),
            }
        }
    }

    pub(crate) impl TCheckedMul<
        T, +Drop<T>, +crate::num::traits::OverflowingMul<T>,
    > of crate::num::traits::CheckedMul<T> {
        fn checked_mul(self: T, v: T) -> Option<T> {
            let (result, overflow) = self.overflowing_mul(v);
            match overflow {
                true => Option::None,
                false => Option::Some(result),
            }
        }
    }
}
