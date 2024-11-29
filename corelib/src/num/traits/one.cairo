//! Traits for types with a multiplicative identity element.

/// Defines a multiplicative identity element for `T`.
///
/// # Laws
///
/// ```text
/// a * 1 = a       ∀ a ∈ T
/// 1 * a = a       ∀ a ∈ T
/// ```
pub trait One<T> {
    /// Returns the multiplicative identity element of `T`, `1`.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::One;
    ///
    /// assert!(One::<u32>::one() == 1);
    /// ```
    fn one() -> T;

    /// Returns true if `self` is equal to the multiplicative identity.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::One;
    ///
    /// assert!(1.is_one());
    /// assert!(!0.is_one());
    /// ```
    fn is_one(self: @T) -> bool;

    /// Returns false if `self` is equal to the multiplicative identity.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::One;
    ///
    /// assert!(0.is_non_one());
    /// assert!(!1.is_non_one());
    /// ```
    fn is_non_one(self: @T) -> bool;
}
