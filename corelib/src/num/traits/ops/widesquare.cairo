//! Wide square operation.
//!
//! This module provides the [`WideSquare`] trait which enables squaring operations
//! that return a result type with double the bit width of the input type.
//! This is particularly useful when you need to square a number without
//! worrying about overflow, as the result type can hold the full range of possible values.
//!
//! # Examples
//!
//! ```
//! use core::num::traits::WideSquare;
//!
//! // Squaring a `u8` value to get a `u16` result
//! let a: u8 = 200;
//! let result: u16 = a.wide_square();
//! assert!(result == 40000);
//!
//! // Squaring a `u128` value to get a `u256` result
//! let x: u128 = 0xffffffffffffffffffffffffffffffff; // max u128
//! let wide_result: u256 = x.wide_square(); // No overflow occurs
//! assert!(wide_result == 0xfffffffffffffffffffffffffffffffe00000000000000000000000000000001);
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

use crate::num::traits::WideMul;

/// A trait for a type that can be squared to produce a wider type.
///
/// This trait enables squaring operations where the result type has double
/// the bit width of the input type, preventing overflow in cases where the
/// result would exceed the input type's maximum value.
///
/// # Examples
///
/// ```
/// use core::num::traits::WideSquare;
///
/// let a: u8 = 16;
/// let result: u16 = a.wide_square();
/// assert!(result == 256);
/// ```
pub trait WideSquare<T> {
    /// The type of the result of the square.
    type Target;
    /// Calculates the square, producing a wider type.
    fn wide_square(self: T) -> Self::Target;
}

mod wide_mul_based {
    pub impl TWideSquare<T, impl TWideMul: super::WideMul<T, T>, +Copy<T>> of super::WideSquare<T> {
        type Target = TWideMul::Target;
        fn wide_square(self: T) -> Self::Target {
            TWideMul::wide_mul(self, self)
        }
    }
}

impl WideSquareI8 = wide_mul_based::TWideSquare<i8>;
impl WideSquareI16 = wide_mul_based::TWideSquare<i16>;
impl WideSquareI32 = wide_mul_based::TWideSquare<i32>;
impl WideSquareI64 = wide_mul_based::TWideSquare<i64>;
impl WideSquareU8 = wide_mul_based::TWideSquare<u8>;
impl WideSquareU16 = wide_mul_based::TWideSquare<u16>;
impl WideSquareU32 = wide_mul_based::TWideSquare<u32>;
impl WideSquareU64 = wide_mul_based::TWideSquare<u64>;
impl WideSquareU128 = wide_mul_based::TWideSquare<u128>;
impl WideSquareU256 of WideSquare<u256> {
    type Target = crate::integer::u512;
    fn wide_square(self: u256) -> Self::Target {
        inner::u256_wide_square(self)
    }
}

mod inner {
    use crate::integer::{u128_add_with_bounded_int_carry, u512, upcast};
    use crate::internal::bounded_int;
    use crate::num::traits::{WideMul, WideSquare, WrappingAdd};

    pub fn u256_wide_square(value: u256) -> u512 {
        let u256 { high: limb1, low: limb0 } = value.low.wide_square();
        let u256 { high: limb2, low: limb1_part } = value.low.wide_mul(value.high);
        let (limb1, limb1_overflow0) = u128_add_with_bounded_int_carry(limb1, limb1_part);
        let (limb1, limb1_overflow1) = u128_add_with_bounded_int_carry(limb1, limb1_part);
        let (limb2, limb2_overflow0) = u128_add_with_bounded_int_carry(limb2, limb2);
        let u256 { high: limb3, low: limb2_part } = value.high.wide_square();
        let (limb2, limb2_overflow1) = u128_add_with_bounded_int_carry(limb2, limb2_part);
        // Packing together the overflow bits, making a cheaper addition into limb2.
        let limb1_overflow = bounded_int::add(limb1_overflow0, limb1_overflow1);
        let (limb2, limb2_overflow2) = u128_add_with_bounded_int_carry(
            limb2, upcast(limb1_overflow),
        );
        // Packing together the overflow bits, making a cheaper addition into limb3.
        let limb2_overflow = bounded_int::add(limb2_overflow0, limb2_overflow1);
        let limb2_overflow = bounded_int::add(limb2_overflow, limb2_overflow2);
        // No overflow since no limb4.
        let limb3 = limb3.wrapping_add(upcast(limb2_overflow));
        u512 { limb0, limb1, limb2, limb3 }
    }
}
