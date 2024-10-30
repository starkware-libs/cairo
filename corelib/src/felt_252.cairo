//! Felt252 module that implements the [`Zero`] and the [`One`] traits for the `felt252` type.
//!
//! ### One Trait
//!
//! - **one()**: Returns the one value of `felt252`.
//! - **is_one()**: Checks if a `felt252` value is one.
//! - **is_non_one()**: Checks if a `felt252` value is not one.
//!
//! ## Examples
//!
//! The `Zero` trait provides three methods: [`Zero::zero`], [`Zero::is_zero`] and
//! [`Zero::is_non_zero`].
//!
//! ```
//! use core::num::traits::Zero;
//!
//! let zero = Zero::<felt252>::zero();
//! assert!(zero.is_zero());
//! assert!(!zero.is_non_zero());
//! ```
//!
//! The `One` trait provides three methods: [`One::one`], [`One::is_one`] and [`One::is_non_one`].
//!
//! ```
//! use core::num::traits::One;
//!
//! let one = One::<felt252>::one();
//! assert!(one.is_one());
//! assert!(!one.is_non_one());
//! ```

/// Implementation of the `Zero` trait for the `felt252` type.
pub(crate) impl Felt252Zero of crate::num::traits::Zero<felt252> {
    /// Returns the zero value of felt252.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::Zero;
    ///
    /// let zero: felt252 = Zero::zero();
    /// let zero = Zero::<felt252>::zero(); // both are equivalent
    /// ```
    fn zero() -> felt252 {
        0
    }

    /// Returns `true` if a given `felt252` is zero, false otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::Zero;
    ///
    /// let zero: felt252 = Zero::zero();
    /// let is_zero = zero.is_zero();
    /// assert!(is_zero);
    /// ```
    #[inline]
    fn is_zero(self: @felt252) -> bool {
        *self == Self::zero()
    }

    /// Returns `true` if a given `felt252` is not zero, false otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::Zero;
    ///
    /// let zero: felt252 = Zero::zero();
    /// let is_non_zero = zero.is_non_zero();
    /// assert!(!is_non_zero);
    /// ```
    #[inline]
    fn is_non_zero(self: @felt252) -> bool {
        !self.is_zero()
    }
}

/// Implementation of the `One` trait for the `felt252` type.
pub(crate) impl Felt252One of crate::num::traits::One<felt252> {
    /// Returns the one value of `felt252`.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::One;
    ///
    /// let one: felt252 = One::one();
    /// let one = One::<felt252>::one(); // both are equivalent
    /// ```
    fn one() -> felt252 {
        1
    }

    /// Returns `true` if a given `felt252` is one, false otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::One;
    ///
    /// let one: felt252 = One::one();
    /// let is_one = one.is_one();
    /// assert!(is_one);
    /// ```
    #[inline]
    fn is_one(self: @felt252) -> bool {
        *self == Self::one()
    }

    /// Returns `true` if a given `felt252` is not one, false otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::One;
    ///
    /// let one: felt252 = One::one();
    /// let is_non_one = one.is_non_one();
    /// assert!(!is_non_one);
    /// ```
    #[inline]
    fn is_non_one(self: @felt252) -> bool {
        !self.is_one()
    }
}
