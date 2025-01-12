//! Trait and implementations for raising a value to a power.
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
/// use core::num::traits::Pow;
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
    /// Helper to implement const pow fns.
    trait ConstPowHelper<T> {
        const fn one() -> T;
        const fn mul(lhs: T, rhs: T) -> T;
    }

    /// Square and multiply implementation for `Pow`.
    pub impl PowByMul<T, impl H: ConstPowHelper<T>, +Copy<T>, +Drop<T>> of super::Pow<T, usize> {
        type Output = T;

        const fn pow(self: T, exp: usize) -> T {
            if exp == 0 {
                H::one()
            } else {
                pow_non_zero_exp(self, exp)
            }
        }
    }

    /// Equivalent of `PowByMul::pow` but assumes `exp` is non zero.
    const fn pow_non_zero_exp<T, impl H: ConstPowHelper<T>, +Copy<T>, +Drop<T>>(
        base: T, exp: usize,
    ) -> T {
        let (tail_exp, head_exp) = DivRem::div_rem(exp, 2);
        if head_exp == 0 {
            pow_non_zero_exp(H::mul(base, base), tail_exp)
        } else {
            pow_given_sqrt_base(base, tail_exp, base)
        }
    }

    /// Returns `(sqrt_base * sqrt_base).pow(exp) * acc`.
    ///
    /// Receives the square root of the base to avoid overflow if the squaring is not required for
    /// the calculation (mostly when `exp` is 0).
    const fn pow_given_sqrt_base<T, impl H: ConstPowHelper<T>, +Copy<T>, +Drop<T>>(
        sqrt_base: T, exp: usize, acc: T,
    ) -> T {
        if exp == 0 {
            acc
        } else {
            let (tail_exp, head_exp) = DivRem::div_rem(exp, 2);
            let base = H::mul(sqrt_base, sqrt_base);
            if head_exp == 0 {
                H::mul(pow_non_zero_exp(H::mul(base, base), tail_exp), acc)
            } else {
                pow_given_sqrt_base(base, tail_exp, H::mul(base, acc))
            }
        }
    }

    // TODO(gil): Use a macro for it instead of copy paste.
    // TODO(orizi): Consider extracting this for other corelib const calculations.
    // Not using a trait for the implementation to allow `fn` to be `const`.

    impl ConstPowHelperFelt252 of ConstPowHelper<felt252> {
        const fn one() -> felt252 {
            1
        }
        const fn mul(lhs: felt252, rhs: felt252) -> felt252 {
            lhs * rhs
        }
    }

    impl ConstPowHelperI8 of ConstPowHelper<i8> {
        const fn one() -> i8 {
            1
        }
        const fn mul(lhs: i8, rhs: i8) -> i8 {
            lhs * rhs
        }
    }

    impl ConstPowHelperU8 of ConstPowHelper<u8> {
        const fn one() -> u8 {
            1
        }
        const fn mul(lhs: u8, rhs: u8) -> u8 {
            lhs * rhs
        }
    }

    impl ConstPowHelperI16 of ConstPowHelper<i16> {
        const fn one() -> i16 {
            1
        }
        const fn mul(lhs: i16, rhs: i16) -> i16 {
            lhs * rhs
        }
    }

    impl ConstPowHelperU16 of ConstPowHelper<u16> {
        const fn one() -> u16 {
            1
        }
        const fn mul(lhs: u16, rhs: u16) -> u16 {
            lhs * rhs
        }
    }

    impl ConstPowHelperI32 of ConstPowHelper<i32> {
        const fn one() -> i32 {
            1
        }
        const fn mul(lhs: i32, rhs: i32) -> i32 {
            lhs * rhs
        }
    }

    impl ConstPowHelperU32 of ConstPowHelper<u32> {
        const fn one() -> u32 {
            1
        }
        const fn mul(lhs: u32, rhs: u32) -> u32 {
            lhs * rhs
        }
    }

    impl ConstPowHelperI64 of ConstPowHelper<i64> {
        const fn one() -> i64 {
            1
        }
        const fn mul(lhs: i64, rhs: i64) -> i64 {
            lhs * rhs
        }
    }

    impl ConstPowHelperU64 of ConstPowHelper<u64> {
        const fn one() -> u64 {
            1
        }
        const fn mul(lhs: u64, rhs: u64) -> u64 {
            lhs * rhs
        }
    }

    impl ConstPowHelperI128 of ConstPowHelper<i128> {
        const fn one() -> i128 {
            1
        }
        const fn mul(lhs: i128, rhs: i128) -> i128 {
            lhs * rhs
        }
    }

    impl ConstPowHelperU128 of ConstPowHelper<u128> {
        const fn one() -> u128 {
            1
        }
        const fn mul(lhs: u128, rhs: u128) -> u128 {
            lhs * rhs
        }
    }

    impl ConstPowHelperU256 of ConstPowHelper<u256> {
        const fn one() -> u256 {
            1
        }
        const fn mul(lhs: u256, rhs: u256) -> u256 {
            lhs * rhs
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
