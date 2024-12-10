//! Traits for types with an additive identity element.

/// Defines an additive identity element for `T`.
///
/// # Laws
///
/// ```text
/// a + 0 = a       ∀ a ∈ T
/// 0 + a = a       ∀ a ∈ T
/// ```
pub trait Zero<T> {
    /// Returns the additive identity element of `T`, `0`.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::Zero;
    ///
    /// assert!(Zero::<u32>::zero() == 0);
    /// ```
    fn zero() -> T;

    /// Returns true if `self` is equal to the additive identity.
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

    /// Returns false if `self` is equal to the additive identity.
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
