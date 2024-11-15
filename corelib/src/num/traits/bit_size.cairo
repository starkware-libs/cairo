//! Provides a trait for retrieving the bit size of any type.

/// Trait used to retrieve the size in bits of a type.
pub trait BitSize<T> {
    /// Returns the size in bits of `T` as `usize`.
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
