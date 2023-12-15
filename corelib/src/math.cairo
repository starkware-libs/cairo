use core::zeroable::{IsZeroResult, NonZeroIntoImpl, Zeroable};
use core::traits::{Into, TryInto};
use core::option::OptionTrait;
use core::integer::{u256_wide_mul, u512_safe_div_rem_by_u256, U128MulGuarantee};

// TODO(yuval): use signed integers once supported.
// TODO(yuval): use a single impl of a trait with associated impls, once associated impls are
// supported.
/// Extended GCD: finds (g, s, t, sub_direction) such that
/// `g = gcd(a, b) = s * a - t * b` if `sub_direction` is true, or
/// `g = gcd(a, b) = t * b - s * a` if `sub_direction` is false.
/// `(s, -t)` or `(-s, t)` are the Bezout coefficients (according to `sub_direction`).
///
/// Uses the Extended Euclidean algorithm.
pub fn egcd<
    T,
    +Copy<T>,
    +Drop<T>,
    +Add<T>,
    +Mul<T>,
    +DivRem<T>,
    +Zeroable<T>,
    +Oneable<T>,
    +TryInto<T, NonZero<T>>,
>(
    a: NonZero<T>, b: NonZero<T>
) -> (T, T, T, bool) {
    let (q, r) = DivRem::<T>::div_rem(a.into(), b);

    if r.is_zero() {
        return (b.into(), Zeroable::zero(), Oneable::one(), false);
    }

    // `sign` (1 for true, -1 for false) is the sign of `g` in the current iteration.
    // 0 is considered negative for this purpose.
    let (g, s, t, sign) = egcd(b, r.try_into().unwrap());
    // We know that `a = q*b + r` and that `s*b - t*r = sign*g`.
    // So `t*a - (s + q*t)*b = t*r - s*b = sign*g`.
    // Thus we pick `new_s = t`, `new_t = s + q*t`, `new_sign = !sign`.
    (g, t, s + q * t, !sign)
}

// TODO(yuval): use signed integers once supported.
/// Returns the inverse of `a` modulo `n`, or None if `gcd(a, n) > 1`.
pub fn inv_mod<
    T,
    +Copy<T>,
    +Drop<T>,
    +Add<T>,
    +Sub<T>,
    +Mul<T>,
    +DivRem<T>,
    +Zeroable<T>,
    +Oneable<T>,
    +TryInto<T, NonZero<T>>,
>(
    a: NonZero<T>, n: NonZero<T>
) -> Option<T> {
    if Oneable::<T>::is_one(n.into()) {
        return Option::Some(Zeroable::zero());
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
    b: u256, n: NonZero<u256>
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
        U128MulGuarantee
    ),
    (U128MulGuarantee, U128MulGuarantee)
> implicits(RangeCheck) nopanic;

/// Returns the inverse of `a` modulo `n`, or None if `a` is not invertible modulo `n`.
/// All `b`s will be considered not invertible for `n == 1`.
#[inline(always)]
pub fn u256_inv_mod(a: u256, n: NonZero<u256>) -> Option<NonZero<u256>> {
    match u256_guarantee_inv_mod_n(a, n) {
        Result::Ok((inv_a, _, _, _, _, _, _, _, _)) => Option::Some(inv_a),
        Result::Err(_) => Option::None(())
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
        T, impl OneImpl: core::num::traits::One<T>, +Drop<T>, +Copy<T>
    > of super::Oneable<T> {
        fn one() -> T {
            OneImpl::one()
        }
        #[inline(always)]
        fn is_one(self: T) -> bool {
            OneImpl::is_one(@self)
        }
        #[inline(always)]
        fn is_non_one(self: T) -> bool {
            OneImpl::is_non_one(@self)
        }
    }
}

// Oneable impls
impl U8Oneable = math::one_based::OneableImpl<u8>;
impl U16Oneable = math::one_based::OneableImpl<u16>;
impl U32Oneable = math::one_based::OneableImpl<u32>;
impl U64Oneable = math::one_based::OneableImpl<u64>;
impl U128Oneable = math::one_based::OneableImpl<u128>;
impl U256Oneable = math::one_based::OneableImpl<u256>;
