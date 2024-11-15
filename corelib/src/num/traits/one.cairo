//! Provides a trait for types with a concept of one that can be compared to one.
//!
//! Useful for numeric types or any type with a multiplicative identity element.

/// `One` trait for types that can be compared to one.
pub trait One<T> {
    /// Returns the multiplicative identity element of `self`, 1.
    ///
    /// This method should return a value that, when multiplied by any other value of type T,
    /// does not change that value.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::One;
    ///
    /// assert!(One::<u32>::one() == 1);
    /// ```
    fn one() -> T;


    /// Returns whether `self` is equal to 1, the multiplicative identity element.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::One;
    ///
    /// assert!(1.is_one());
    /// assert!(!5.is_one());
    /// ```
    fn is_one(self: @T) -> bool;

    /// Returns whether `self` is not equal to 1, the multiplicative identity element.
    ///
    /// This method is the logical inverse of `is_one()`.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::One;
    ///
    /// assert!(5.is_non_one());
    /// assert!(!1.is_non_one());
    /// ```
    fn is_non_one(self: @T) -> bool;
}
