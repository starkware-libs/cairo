//! Trait for performing multiplication that results in a wider type.
//!
//! This module provides the [`WideMul`] trait which enables multiplication operations
//! that return a result type with double the bit width of the input types.
//! This is particularly useful when you need to perform multiplication without
//! worrying about overflow, as the result type can hold the full range of possible values.
//!
//! # Examples
//!
//! ```
//! use core::num::traits::WideMul;
//!
//! // Multiplying two `u8` values to get a `u16` result
//! let a: u8 = 200;
//! let b: u8 = 100;
//! let result: u16 = a.wide_mul(b);
//! assert!(result == 20000);
//!
//! // Multiplying two `u128` values to get a `u256` result
//! let x: u128 = 0xffffffffffffffffffffffffffffffff; // max u128
//! let y: u128 = 2;
//! let wide_result = x.wide_mul(y); // No overflow occurs
//! assert!(wide_result == 0x01fffffffffffffffffffffffffffffffe);
//! ```
//!
//! # Available Implementations
//!
//! The trait is implemented for the following type pairs:
//! - `i8` → `i16`
//! - `i16` → `i32`
//! - `i32` → `i64`
//! - `i64` → `i128`
//! - `u8` → `u16`
//! - `u16` → `u32`
//! - `u32` → `u64`
//! - `u64` → `u128`
//! - `u128` → `u256`
//! - `u256` → `u512`

/// A trait for types that can be multiplied together to produce a wider type.
///
/// This trait enables multiplication operations where the result type has double
/// the bit width of the input types, preventing overflow in cases where the
/// result would exceed the input type's maximum value.
///
/// # Examples
///
/// ```
/// use core::num::traits::WideMul;
///
/// let a: u8 = 255; // maximum value for u8
/// let b: u8 = 255;
/// let result: u16 = a.wide_mul(b);
/// assert!(result == 65025);
/// ```
pub trait WideMul<Lhs, Rhs> {
    /// The type of the result of the multiplication.
    type Target;
    /// Multiply two values together, producing a wider type.
    fn wide_mul(self: Lhs, other: Rhs) -> Self::Target;
}

impl WideMulI8 of WideMul<i8, i8> {
    type Target = i16;
    fn wide_mul(self: i8, other: i8) -> i16 {
        crate::integer::i8_wide_mul(self, other)
    }
}

impl WideMulI16 of WideMul<i16, i16> {
    type Target = i32;
    fn wide_mul(self: i16, other: i16) -> i32 {
        crate::integer::i16_wide_mul(self, other)
    }
}

impl WideMulI32 of WideMul<i32, i32> {
    type Target = i64;
    fn wide_mul(self: i32, other: i32) -> i64 {
        crate::integer::i32_wide_mul(self, other)
    }
}

impl WideMulI64 of WideMul<i64, i64> {
    type Target = i128;
    fn wide_mul(self: i64, other: i64) -> i128 {
        crate::integer::i64_wide_mul(self, other)
    }
}

impl WideMulU8 of WideMul<u8, u8> {
    type Target = u16;
    fn wide_mul(self: u8, other: u8) -> u16 {
        crate::integer::u8_wide_mul(self, other)
    }
}

impl WideMulU16 of WideMul<u16, u16> {
    type Target = u32;
    fn wide_mul(self: u16, other: u16) -> u32 {
        crate::integer::u16_wide_mul(self, other)
    }
}

impl WideMulU32 of WideMul<u32, u32> {
    type Target = u64;
    fn wide_mul(self: u32, other: u32) -> u64 {
        crate::integer::u32_wide_mul(self, other)
    }
}

impl WideMulU64 of WideMul<u64, u64> {
    type Target = u128;
    fn wide_mul(self: u64, other: u64) -> u128 {
        crate::integer::u64_wide_mul(self, other)
    }
}

impl WideMulU128 of WideMul<u128, u128> {
    type Target = u256;
    fn wide_mul(self: u128, other: u128) -> u256 {
        let (high, low) = crate::integer::u128_wide_mul(self, other);
        u256 { low, high }
    }
}

impl WideMulU256 of WideMul<u256, u256> {
    type Target = crate::integer::u512;
    fn wide_mul(self: u256, other: u256) -> crate::integer::u512 {
        crate::integer::u256_wide_mul(self, other)
    }
}
