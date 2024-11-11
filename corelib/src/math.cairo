//! Math module that provides a set of mathematical utilities and functions for working with large
//! integers.
//!
//! ## Extended GCD
//!
//! The `egcd` function computes the extended GCD of two numbers `a` and `b`, returning the GCD `g`
//! and the Bezout coefficients `s` and `t` such that `g = s*a - t*b` or `g = t*b - s*a`, depending
//! on the `sub_direction` flag.
//!
//! ## Modular Inverse
//!
//! The `inv_mod` function computes the modular inverse of a number `a` modulo a given modulus `n`,
//! if it exists. The inverse is guaranteed to be between 1 and `n-1` (inclusive).
//!
//! ## Modular Division and Multiplication
//!
//! The `u256_div_mod_n` function performs modular division, computing `a / b (mod n)`, if `b` is
//! invertible modulo `n`.
//! The `u256_mul_mod_n` function performs modular multiplication, computing `a * b (mod n)`.
//!
//! ## Oneable Trait
//!
//! The `Oneable` trait defines the behavior for types that have a multiplicative identity element
//! (1). It provides methods for getting the identity element, checking if a value is equal to 1,
//! and checking if a value is not equal to 1.

#[allow(unused_imports)]
use crate::zeroable::{IsZeroResult, NonZeroIntoImpl, Zeroable};
#[allow(unused_imports)]
use crate::traits::{Into, TryInto};
#[allow(unused_imports)]
use crate::option::OptionTrait;
use crate::integer::{u256_wide_mul, u512_safe_div_rem_by_u256, U128MulGuarantee};
use crate::RangeCheck;

// TODO(yuval): use signed integers once supported.
// TODO(yuval): use a single impl of a trait with associated impls, once associated impls are
// supported.
/// Extended GCD: finds (g, s, t, sub_direction) such that:
/// `g = gcd(a, b) = s * a - t * b` if `sub_direction` is true, or
/// `g = gcd(a, b) = t * b - s * a` if `sub_direction` is false.
/// `(s, -t)` or `(-s, t)` are the Bezout coefficients (according to `sub_direction`).
/// Uses the Extended Euclidean algorithm.
///
/// # Examples
///
/// ```
/// use core::math::egcd;
///
/// let x : NonZero<u32> = 10;
/// let y : NonZero<u32> = 15;
///
/// let (result, _, _, _) = egcd(x, y);
/// assert!(result == 5);
/// ```
pub fn egcd<
    T,
    +Copy<T>,
    +Drop<T>,
    +Add<T>,
    +Mul<T>,
    +DivRem<T>,
    +core::num::traits::Zero<T>,
    +core::num::traits::One<T>,
    +TryInto<T, NonZero<T>>,
>(
    a: NonZero<T>, b: NonZero<T>,
) -> (T, T, T, bool) {
    let (q, r) = DivRem::<T>::div_rem(a.into(), b);

    let r = if let Option::Some(r) = r.try_into() {
        r
    } else {
        return (b.into(), core::num::traits::Zero::zero(), core::num::traits::One::one(), false);
    };

    // `sign` (1 for true, -1 for false) is the sign of `g` in the current iteration.
    // 0 is considered negative for this purpose.
    let (g, s, t, sign) = egcd(b, r);
    // We know that `a = q*b + r` and that `s*b - t*r = sign*g`.
    // So `t*a - (s + q*t)*b = t*r - s*b = sign*g`.
    // Thus we pick `new_s = t`, `new_t = s + q*t`, `new_sign = !sign`.
    (g, t, s + q * t, !sign)
}

// TODO(yuval): use signed integers once supported.
/// Computes `s` the inverse of `a` modulo `n` such that `as`≡ 1 modulo `n`, or None if `gcd(a, n)
/// > 1`. `s` is guaranteed to be between `1` and `n - 1` (inclusive).
/// Returns `s` or `n - s` depending on the `sub_direction` when computing `egcd(a, n)`.
///
/// # Examples
///
/// ```
/// use core::math::inv_mod;
///
/// let a : NonZero<u32> = 17;
/// let n : NonZero<u32> = 29;
///
/// let result = inv_mod(a, n);
/// assert!(result.uwrap() == 12);
/// ```
pub fn inv_mod<
    T,
    +Copy<T>,
    +Drop<T>,
    +Add<T>,
    +Sub<T>,
    +Mul<T>,
    +DivRem<T>,
    +core::num::traits::Zero<T>,
    +core::num::traits::One<T>,
    +TryInto<T, NonZero<T>>,
>(
    a: NonZero<T>, n: NonZero<T>,
) -> Option<T> {
    if core::num::traits::One::<T>::is_one(@n.into()) {
        return Option::Some(core::num::traits::Zero::zero());
    }
    let (g, s, _, sub_direction) = egcd(a, n);
    if g.is_one() {
        // `1 = g = gcd(a, n) = +-(s*a - t*n) => s*a = +-1 (mod n)`.
        // The absolute values of Bezout coefficients are guaranteed to be `< n`.
        // With n > 1 and gcd = 1, `s` can't be 0.
        if sub_direction {
            // `s` is the Bezout coefficient, `0 < s < n`.
            Option::Some(s)
        } else {
            // `-s` is the Bezout coefficient.
            // `-n < -s < 0 => 0 < n - s < n`, and `n - s = -s (mod n)`.
            Option::Some(n.into() - s)
        }
    } else {
        Option::None
    }
}

/// Returns `1 / b (mod n)`, or None if `b` is not invertible modulo `n`.
/// All `b`s will be considered not invertible for `n == 1`.
/// Additionally returns several `U128MulGuarantee`s that are required for validating the
/// calculation.
extern fn u256_guarantee_inv_mod_n(
    b: u256, n: NonZero<u256>,
) -> Result<
    (
        NonZero<u256>,
        U128MulGuarantee,
        U128MulGuarantee,
        U128MulGuarantee,
        U128MulGuarantee,
        U128MulGuarantee,
        U128MulGuarantee,
        U128MulGuarantee,
        U128MulGuarantee,
    ),
    (U128MulGuarantee, U128MulGuarantee),
> implicits(RangeCheck) nopanic;

/// Returns the inverse of `a` modulo `n`, or None if `a` is not invertible modulo `n`.
/// All `a`s will be considered not invertible for `n == 1`.
#[inline]
pub fn u256_inv_mod(a: u256, n: NonZero<u256>) -> Option<NonZero<u256>> {
    match u256_guarantee_inv_mod_n(a, n) {
        Result::Ok((inv_a, _, _, _, _, _, _, _, _)) => Option::Some(inv_a),
        Result::Err(_) => Option::None(()),
    }
}

/// Returns `a / b (mod n)`, or None if `b` is not invertible modulo `n`.
pub fn u256_div_mod_n(a: u256, b: u256, n: NonZero<u256>) -> Option<u256> {
    Option::Some(u256_mul_mod_n(a, u256_inv_mod(b, n)?.into(), n))
}

/// Returns `a * b (mod n)`.
pub fn u256_mul_mod_n(a: u256, b: u256, n: NonZero<u256>) -> u256 {
    let (_, r) = u512_safe_div_rem_by_u256(u256_wide_mul(a, b), n);
    r
}

// === Oneable ===

/// A trait for types that have a multiplicative identity element.
trait Oneable<T> {
    /// Returns the multiplicative identity element of Self, 1.
    #[must_use]
    fn one() -> T;
    /// Returns whether self is equal to 1, the multiplicative identity element.
    #[must_use]
    fn is_one(self: T) -> bool;
    /// Returns whether self is not equal to 1, the multiplicative identity element.
    #[must_use]
    fn is_non_one(self: T) -> bool;
}

pub(crate) mod one_based {
    pub(crate) impl OneableImpl<
        T, impl OneImpl: crate::num::traits::One<T>, +Drop<T>, +Copy<T>,
    > of super::Oneable<T> {
        fn one() -> T {
            OneImpl::one()
        }
        #[inline]
        fn is_one(self: T) -> bool {
            OneImpl::is_one(@self)
        }
        #[inline]
        fn is_non_one(self: T) -> bool {
            OneImpl::is_non_one(@self)
        }
    }
}

// Oneable impls
impl U8Oneable = one_based::OneableImpl<u8>;
impl U16Oneable = one_based::OneableImpl<u16>;
impl U32Oneable = one_based::OneableImpl<u32>;
impl U64Oneable = one_based::OneableImpl<u64>;
impl U128Oneable = one_based::OneableImpl<u128>;
impl U256Oneable = one_based::OneableImpl<u256>;
