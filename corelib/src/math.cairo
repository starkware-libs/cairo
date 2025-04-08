//! Mathematical operations and utilities.
//!
//! Provides extended GCD, modular inverse, and modular arithmetic operations.

use crate::RangeCheck;
use crate::integer::{U128MulGuarantee, u256_wide_mul, u512_safe_div_rem_by_u256};
#[allow(unused_imports)]
use crate::option::OptionTrait;
#[allow(unused_imports)]
use crate::traits::{Into, TryInto};
#[allow(unused_imports)]
use crate::zeroable::{IsZeroResult, NonZeroIntoImpl, Zeroable};

// TODO(yuval): use signed integers once supported.
// TODO(yuval): use a single impl of a trait with associated impls, once associated impls are
// supported.
/// Computes the extended GCD and Bezout coefficients for two numbers.
///
/// Uses the Extended Euclidean algorithm to find (g, s, t, sub_direction) where `g = gcd(a, b)`.
/// The relationship between inputs and outputs is:
/// * If `sub_direction` is true:  `g = s * a - t * b`
/// * If `sub_direction` is false: `g = t * b - s * a`
///
/// Returns a tuple (g, s, t, sub_direction) where g is the GCD and `(s, -t)` or `(-s, t)` are the
/// Bezout coefficients (according to `sub_direction`).
///
/// # Examples
///
/// ```
/// use core::math::egcd;
///
/// let (g, s, t, dir) = egcd::<u32>(12, 8);
/// assert!(g == 4);
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

    let r = if let Some(r) = r.try_into() {
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
/// Computes the modular multiplicative inverse of `a` modulo `n`.
///
/// Returns `s` such that `a*s ≡ 1 (mod n)` where `s` is between `1` and `n-1` inclusive, or
/// `None` if `gcd(a,n) > 1` (inverse doesn't exist).
///
/// # Examples
///
/// ```
/// use core::math::inv_mod;
///
/// let inv = inv_mod::<u32>(3, 7);
/// assert!(inv == Some(5));
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
        return Some(core::num::traits::Zero::zero());
    }
    let (g, s, _, sub_direction) = egcd(a, n);
    if g.is_one() {
        // `1 = g = gcd(a, n) = +-(s*a - t*n) => s*a = +-1 (mod n)`.
        // The absolute values of Bezout coefficients are guaranteed to be `< n`.
        // With n > 1 and gcd = 1, `s` can't be 0.
        if sub_direction {
            // `s` is the Bezout coefficient, `0 < s < n`.
            Some(s)
        } else {
            // `-s` is the Bezout coefficient.
            // `-n < -s < 0 => 0 < n - s < n`, and `n - s = -s (mod n)`.
            Some(n.into() - s)
        }
    } else {
        None
    }
}

/// Returns `1 / b (mod n)`, or `None` if `b` is not invertible modulo `n`.
///
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

/// Returns the inverse of `a` modulo `n`, or `None` if `a` is not invertible modulo `n`.
///
/// All `a`s will be considered not invertible for `n == 1`.
///
/// # Examples
///
/// ```
/// use core::math::u256_inv_mod;
///
/// let inv = u256_inv_mod(3, 17);
/// assert!(inv == Some(6));
/// ```
#[inline]
pub fn u256_inv_mod(a: u256, n: NonZero<u256>) -> Option<NonZero<u256>> {
    match u256_guarantee_inv_mod_n(a, n) {
        Ok((inv_a, _, _, _, _, _, _, _, _)) => Some(inv_a),
        Err(_) => None(()),
    }
}

/// Returns `a / b (mod n)`, or `None` if `b` is not invertible modulo `n`.
///
/// # Examples
///
/// ```
/// use core::math::u256_inv_mod;
///
/// let result = u256_div_mod_n(17, 7, 29);
/// assert!(result == Some(19));
/// ```
pub fn u256_div_mod_n(a: u256, b: u256, n: NonZero<u256>) -> Option<u256> {
    Some(u256_mul_mod_n(a, u256_inv_mod(b, n)?.into(), n))
}

/// Returns `a * b (mod n)`.
///
/// # Examples
///
/// ```
/// use core::math::u256_mul_mod_n;
///
/// let result = u256_mul_mod_n(17, 23, 29);
/// assert!(result == 14);
/// ```
pub fn u256_mul_mod_n(a: u256, b: u256, n: NonZero<u256>) -> u256 {
    let (_, r) = u512_safe_div_rem_by_u256(u256_wide_mul(a, b), n);
    r
}

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

impl U8Oneable = one_based::OneableImpl<u8>;
impl U16Oneable = one_based::OneableImpl<u16>;
impl U32Oneable = one_based::OneableImpl<u32>;
impl U64Oneable = one_based::OneableImpl<u64>;
impl U128Oneable = one_based::OneableImpl<u128>;
impl U256Oneable = one_based::OneableImpl<u256>;
