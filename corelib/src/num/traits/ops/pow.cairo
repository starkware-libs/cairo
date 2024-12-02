//! Power calculation trait and implementations.
//!
//! This module provides a generic trait and implementation for calculating
//! exponentiation across multiple numeric types with logarithmic time complexity.

/// A trait for calculating a base to the power of an exponent.
pub trait Pow<Base, Exp> {
    /// The type of the result of the power calculation.
    type Output;

    /// Returns `self` to the power `exp`.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::ops::pow::Pow;
    ///
    /// assert!(10_u8.pow(2) == 100);
    /// ```
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
