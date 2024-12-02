//! Traits and implementations for raising a value to a power.
//!
//! This module provides efficient exponentiation operations for numeric types using
//! the square-and-multiply algorithm, which achieves logarithmic time complexity O(log n).

/// Raises a value to the power of `exp`.
///
/// Note that `0⁰` (`pow(0, 0)`) returns `1`. Mathematically this is undefined.
///
/// # Panics
///
/// Panics if the result of the exponentiation operation overflows the output type.
///
/// # Examples
///
/// ```
/// use core::num::traits::ops::Pow;
///
/// assert!(2_i8.pow(4_usize) == 16_i8);
/// assert!(6_u8.pow(3_usize) == 216_u8);
/// assert!(0_u8.pow(0_usize) == 1_u8);
/// ```
pub trait Pow<Base, Exp> {
    /// The type of the result of the power calculation.
    type Output;

    /// Returns `self` to the power `exp`.
    fn pow(self: Base, exp: Exp) -> Self::Output;
}

mod mul_based {
    /// Square and multiply implementation for `Pow`.
    pub impl PowByMul<
        Base, +Mul<Base>, +Copy<Base>, +Drop<Base>, +core::num::traits::One<Base>,
    > of super::Pow<Base, usize> {
        type Output = Base;

        fn pow(self: Base, exp: usize) -> Base {
            let (tail_exp, head_exp) = DivRem::div_rem(exp, 2);
            let tail_result = if tail_exp == 0 {
                core::num::traits::One::one()
            } else {
                Self::pow(self * self, tail_exp)
            };
            if head_exp == 0 {
                tail_result
            } else {
                tail_result * self
            }
        }
    }
}

impl PowFelt252 = mul_based::PowByMul<felt252>;
impl PowI8 = mul_based::PowByMul<i8>;
impl PowU8 = mul_based::PowByMul<u8>;
impl PowI16 = mul_based::PowByMul<i16>;
impl PowU16 = mul_based::PowByMul<u16>;
impl PowI32 = mul_based::PowByMul<i32>;
impl PowU32 = mul_based::PowByMul<u32>;
impl PowI64 = mul_based::PowByMul<i64>;
impl PowU64 = mul_based::PowByMul<u64>;
impl PowI128 = mul_based::PowByMul<i128>;
impl PowU128 = mul_based::PowByMul<u128>;
impl PowU256 = mul_based::PowByMul<u256>;
