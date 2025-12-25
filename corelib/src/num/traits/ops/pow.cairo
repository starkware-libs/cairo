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

    /// Square-and-multiply implementation for `Pow`.
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

    /// Equivalent of `PowByMul::pow` but assumes `exp` is non-zero.
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

    /// Macro for generating an implementation for `ConstPowHelper`.
    /// Not using a trait for the implementation to allow `fn` to be `const`.
    macro impl_const_pow_helper {
        ($impl_name: ident, $ty: ident) => {
            impl $impl_name of $defsite::ConstPowHelper<$ty> {
                const fn one() -> $ty {
                    1
                }
                const fn mul(lhs: $ty, rhs: $ty) -> $ty {
                    lhs * rhs
                }
            }
        };
    }
    impl_const_pow_helper!(ConstPowHelperFelt252, felt252);
    impl_const_pow_helper!(ConstPowHelperI8, i8);
    impl_const_pow_helper!(ConstPowHelperU8, u8);
    impl_const_pow_helper!(ConstPowHelperI16, i16);
    impl_const_pow_helper!(ConstPowHelperU16, u16);
    impl_const_pow_helper!(ConstPowHelperI32, i32);
    impl_const_pow_helper!(ConstPowHelperU32, u32);
    impl_const_pow_helper!(ConstPowHelperI64, i64);
    impl_const_pow_helper!(ConstPowHelperU64, u64);
    impl_const_pow_helper!(ConstPowHelperI128, i128);
    impl_const_pow_helper!(ConstPowHelperU128, u128);
    impl_const_pow_helper!(ConstPowHelperU256, u256);
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
