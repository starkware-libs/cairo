use zeroable::{IsZeroResult, NonZeroIntoImpl, Zeroable};
use traits::{Into, TryInto};
use option::OptionTrait;

// TODO(yuval): use signed integers once supported.
// TODO(yuval): use a single impl of a trait with associated impls, once associated impls are
// supported.
/// Extended GCD: finds (g, s, t, sub_direction) such that
/// `g = gcd(a, b) = s * a - t * b` if `sub_direction` is true, or
/// `g = gcd(a, b) = t * b - s * a` if `sub_direction` is false.
/// `(s, -t)` or `(-s, t)` are the Bezout coefficients (according to `sub_direction`).
///
/// Uses the Extended Euclidean algorithm.
fn egcd<
    T,
    impl TCopyImpl: Copy<T>,
    impl TDropImpl: Drop<T>,
    impl TAddImpl: Add<T>,
    impl TMulImpl: Mul<T>,
    impl TDivRemImpl: DivRem<T>,
    impl TZeroableImpl: Zeroable<T>,
    impl TOneableImpl: Oneable<T>,
    impl TTryIntoNonZeroImpl: TryInto<T, NonZero<T>>,
>(
    a: NonZero<T>, b: NonZero<T>
) -> (T, T, T, bool) {
    let (q, r) = TDivRemImpl::div_rem(a.into(), b);

    if r.is_zero() {
        return (b.into(), TZeroableImpl::zero(), TOneableImpl::one(), false);
    }

    // `sign` (1 for true, -1 for false) is the sign of `g` in the current iteration.
    // 0 is considered negative for this purpose.
    let (g, s, t, sign) = egcd(b, r.try_into().unwrap());
    // We know that `a = q*b + r` and that `s*b - t*r = sign*g`.
    // So `t*a - (s + q*t)*b = t*r - s*b = sign*g`.
    // Thus we pick `new_s = t`, `new_t = s + q*t`, `new_sign = !sign`.
    (g, t, s + q * t, !sign)
}

// === Oneable ===

trait Oneable<T> {
    /// Returns the multiplicative identity element of Self, 1.
    fn one() -> T;
    /// Returns whether self is equal to 1, the multiplicative identity element.
    fn is_one(self: T) -> bool;
    /// Returns whether self is not equal to 1, the multiplicative identity element.
    fn is_non_one(self: T) -> bool;
}

impl U8Oneable of Oneable<u8> {
    fn one() -> u8 {
        1
    }
    #[inline(always)]
    fn is_one(self: u8) -> bool {
        self == U8Oneable::one()
    }
    #[inline(always)]
    fn is_non_one(self: u8) -> bool {
        self != U8Oneable::one()
    }
}

impl U16Oneable of Oneable<u16> {
    fn one() -> u16 {
        1
    }
    #[inline(always)]
    fn is_one(self: u16) -> bool {
        self == U16Oneable::one()
    }
    #[inline(always)]
    fn is_non_one(self: u16) -> bool {
        self != U16Oneable::one()
    }
}

impl U32Oneable of Oneable<u32> {
    fn one() -> u32 {
        1
    }
    #[inline(always)]
    fn is_one(self: u32) -> bool {
        self == U32Oneable::one()
    }
    #[inline(always)]
    fn is_non_one(self: u32) -> bool {
        self != U32Oneable::one()
    }
}

impl U64Oneable of Oneable<u64> {
    fn one() -> u64 {
        1
    }
    #[inline(always)]
    fn is_one(self: u64) -> bool {
        self == U64Oneable::one()
    }
    #[inline(always)]
    fn is_non_one(self: u64) -> bool {
        self != U64Oneable::one()
    }
}

impl U128Oneable of Oneable<u128> {
    fn one() -> u128 {
        1
    }
    #[inline(always)]
    fn is_one(self: u128) -> bool {
        self == U128Oneable::one()
    }
    #[inline(always)]
    fn is_non_one(self: u128) -> bool {
        self != U128Oneable::one()
    }
}

impl U256Oneable of Oneable<u256> {
    fn one() -> u256 {
        1
    }
    #[inline(always)]
    fn is_one(self: u256) -> bool {
        self == U256Oneable::one()
    }
    #[inline(always)]
    fn is_non_one(self: u256) -> bool {
        self != U256Oneable::one()
    }
}
