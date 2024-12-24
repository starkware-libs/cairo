//! Assignment operator traits for arithmetic operations.
//!
//! This module provides traits for implementing assignment operators like `+=`, `-=`,
//! `*=`, `/=` and `%=`. These traits allow types to define how they handle
//! arithmetic operations that modify values in place.

/// The addition assignment operator `+=`.
pub trait AddAssign<Lhs, Rhs> {
    /// Performs the `+=` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut x: u8 = 3;
    /// x += x;
    /// assert!(x == 6);
    /// ```
    fn add_assign(ref self: Lhs, rhs: Rhs);
}

/// The subtraction assignment operator `-=`.
pub trait SubAssign<Lhs, Rhs> {
    /// Performs the `-=` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut x: u8 = 3;
    /// x -= x;
    /// assert!(x == 0);
    /// ```
    fn sub_assign(ref self: Lhs, rhs: Rhs);
}

/// The multiplication assignment operator `*=`.
pub trait MulAssign<Lhs, Rhs> {
    /// Performs the `*=` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut x: u8 = 3;
    /// x *= x;
    /// assert!(x == 9);
    /// ```
    fn mul_assign(ref self: Lhs, rhs: Rhs);
}

/// The division assignment operator `/=`.
pub trait DivAssign<Lhs, Rhs> {
    /// Performs the `/=` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut x: u8 = 3;
    /// x /= x;
    /// assert!(x == 1);
    /// ```
    fn div_assign(ref self: Lhs, rhs: Rhs);
}

/// The remainder assignment operator `%=`.
pub trait RemAssign<Lhs, Rhs> {
    /// Performs the `%=` operation.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut x: u8 = 3;
    /// x %= x;
    /// assert!(x == 0);
    /// ```
    fn rem_assign(ref self: Lhs, rhs: Rhs);
}
#[feature("deprecated-op-assign-traits")]
use crate::traits::{AddEq, DivEq, MulEq, RemEq, SubEq};

impl DeprecatedAddAssign<T, impl Deprecated: AddEq<T>> of AddAssign<T, T> {
    fn add_assign(ref self: T, rhs: T) {
        Deprecated::add_eq(ref self, rhs)
    }
}

impl DeprecatedSubAssign<T, impl Deprecated: SubEq<T>> of SubAssign<T, T> {
    fn sub_assign(ref self: T, rhs: T) {
        Deprecated::sub_eq(ref self, rhs)
    }
}

impl DeprecatedMulAssign<T, impl Deprecated: MulEq<T>> of MulAssign<T, T> {
    fn mul_assign(ref self: T, rhs: T) {
        Deprecated::mul_eq(ref self, rhs)
    }
}

impl DeprecatedDivAssign<T, impl Deprecated: DivEq<T>> of DivAssign<T, T> {
    fn div_assign(ref self: T, rhs: T) {
        Deprecated::div_eq(ref self, rhs)
    }
}

impl DeprecatedRemAssign<T, impl Deprecated: RemEq<T>> of RemAssign<T, T> {
    fn rem_assign(ref self: T, rhs: T) {
        Deprecated::rem_eq(ref self, rhs)
    }
}
