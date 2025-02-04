//! Utilities for determining the bit size of types.

/// A trait used to retrieve the size of a type in bits.
pub trait BitSize<T> {
    /// Returns the bit size of `T` as a `usize`.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::BitSize;
    ///
    /// let bits = BitSize::<u8>::bits();
    /// assert!(bits == 8);
    /// ```
    #[must_use]
    fn bits() -> usize;
}
