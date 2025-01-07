//! Square root operation for unsigned numeric types.

/// A trait for computing the square root of a number.
///
/// # Examples
///
/// ```
/// use core::num::traits::Sqrt;
///
/// assert!(9_u8.sqrt() == 3);
/// ```
pub trait Sqrt<T> {
    /// The type of the result of the square root operation.
    type Target;
    /// Computes the square root of a number.
    fn sqrt(self: T) -> Self::Target;
}

impl SqrtU8 of Sqrt<u8> {
    type Target = u8;
    fn sqrt(self: u8) -> u8 {
        crate::integer::u8_sqrt(self)
    }
}

impl SqrtU16 of Sqrt<u16> {
    type Target = u8;
    fn sqrt(self: u16) -> u8 {
        crate::integer::u16_sqrt(self)
    }
}

impl SqrtU32 of Sqrt<u32> {
    type Target = u16;
    fn sqrt(self: u32) -> u16 {
        crate::integer::u32_sqrt(self)
    }
}

impl SqrtU64 of Sqrt<u64> {
    type Target = u32;
    fn sqrt(self: u64) -> u32 {
        crate::integer::u64_sqrt(self)
    }
}

impl SqrtU128 of Sqrt<u128> {
    type Target = u64;
    fn sqrt(self: u128) -> u64 {
        crate::integer::u128_sqrt(self)
    }
}

impl SqrtU256 of Sqrt<u256> {
    type Target = u128;
    fn sqrt(self: u256) -> u128 {
        crate::integer::u256_sqrt(self)
    }
}
