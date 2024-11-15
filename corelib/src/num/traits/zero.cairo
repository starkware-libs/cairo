//! Provides a trait for types with a concept of zero that can be compared to zero.
//!
//! Useful for numeric types or any type with an additive identity element.

/// `Zero` trait for types that can be compared to zero.
pub trait Zero<T> {
    /// Returns the additive identity element of `self`, 0.
    ///
    /// This method should return a value that, when added to any other value of type `T`,
    /// does not change that value.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::Zero;
    ///
    /// assert!(Zero::<u32>::zero() == 0);
    /// ```
    fn zero() -> T;

    /// Returns whether `self` is equal to 0, the additive identity element.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::Zero;
    ///
    /// assert!(0.is_zero());
    /// assert!(!5.is_zero());
    /// ```
    fn is_zero(self: @T) -> bool;

    /// Returns whether `self` is not equal to 0, the additive identity element.
    ///
    /// This method is the logical inverse of `is_zero()`.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::Zero;
    ///
    /// assert!(5.is_non_zero());
    /// assert!(!0.is_non_zero());
    /// ```
    fn is_non_zero(self: @T) -> bool;
}
