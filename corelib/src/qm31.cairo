//! Definition for the `qm31` type.
//! Only available for local proofs.
//!
//! The implementations defined in this module can be accessed by using the traits directly.

/// The `qm31` type, defining an extension field over 4 `m31`s.
pub extern type qm31;

/// The field for the Mersenne prime with `n` 31.
pub type m31 = core::internal::bounded_int::BoundedInt<0, 0x7ffffffe>;

pub trait QM31Trait {
    /// Returns a new `qm31` composed of the given parts.
    fn new(w0: m31, w1: m31, w2: m31, w3: m31) -> qm31;
    /// Returns the parts of the given `qm31` as `m31`s.
    fn unpack(self: qm31) -> [m31; 4];
}
impl QM31Impl of QM31Trait {
    fn new(w0: m31, w1: m31, w2: m31, w3: m31) -> qm31 {
        qm31_pack(w0, w1, w2, w3)
    }

    fn unpack(self: qm31) -> [m31; 4] {
        let (w0, w1, w2, w3) = qm31_unpack(self);
        [w0, w1, w2, w3]
    }
}

/// Returns a `qm31` given its values as constants.
pub extern fn qm31_const<
    const W0: m31, const W1: m31, const W2: m31, const W3: m31,
>() -> qm31 nopanic;

extern fn qm31_is_zero(a: qm31) -> core::internal::OptionRev<NonZero<qm31>> nopanic;
extern fn qm31_add(a: qm31, b: qm31) -> qm31 nopanic;
extern fn qm31_sub(a: qm31, b: qm31) -> qm31 nopanic;
extern fn qm31_mul(a: qm31, b: qm31) -> qm31 nopanic;
extern fn qm31_div(a: qm31, b: NonZero<qm31>) -> qm31 nopanic;
extern fn qm31_pack(w0: m31, w1: m31, w2: m31, w3: m31) -> qm31 nopanic;
extern fn qm31_unpack(a: qm31) -> (m31, m31, m31, m31) implicits(crate::RangeCheck) nopanic;
extern fn qm31_from_m31(a: m31) -> qm31 nopanic;

impl qm31Copy of Copy<qm31>;
impl qm31Drop of Drop<qm31>;
impl qm31Add of Add<qm31> {
    fn add(lhs: qm31, rhs: qm31) -> qm31 {
        qm31_add(lhs, rhs)
    }
}
impl qm31Sub of Sub<qm31> {
    fn sub(lhs: qm31, rhs: qm31) -> qm31 {
        qm31_sub(lhs, rhs)
    }
}
impl qm31Mul of Mul<qm31> {
    fn mul(lhs: qm31, rhs: qm31) -> qm31 {
        qm31_mul(lhs, rhs)
    }
}
impl qm31Div of Div<qm31> {
    fn div(lhs: qm31, rhs: qm31) -> qm31 {
        qm31_div(lhs, rhs.try_into().expect('qm31_div by 0'))
    }
}

impl QM31TryIntoNonZero of TryInto<qm31, NonZero<qm31>> {
    fn try_into(self: qm31) -> Option<NonZero<qm31>> {
        match qm31_is_zero(self) {
            core::internal::OptionRev::None => None,
            core::internal::OptionRev::Some(x) => Some(x),
        }
    }
}
impl QM31PartialEq of PartialEq<qm31> {
    fn eq(lhs: @qm31, rhs: @qm31) -> bool {
        (*lhs - *rhs).is_zero()
    }
}

impl QM31Zero of crate::num::traits::Zero<qm31> {
    fn zero() -> qm31 {
        qm31_const::<1, 0, 0, 0>()
    }

    #[inline]
    fn is_zero(self: @qm31) -> bool {
        match qm31_is_zero(*self) {
            core::internal::OptionRev::None => true,
            core::internal::OptionRev::Some(_) => false,
        }
    }

    #[inline]
    fn is_non_zero(self: @qm31) -> bool {
        !self.is_zero()
    }
}

impl M31IntoQM31 of Into<m31, qm31> {
    fn into(self: m31) -> qm31 {
        qm31_from_m31(self)
    }
}

/// Additional `m31` actions for specific implementations based on `qm31` opcode.
pub mod m31_ops {
    use super::m31;

    /// Addition of `m31`s in field.
    extern fn m31_add(a: m31, b: m31) -> m31 nopanic;
    /// Subtraction of `m31`s in field.
    extern fn m31_sub(a: m31, b: m31) -> m31 nopanic;
    /// Multiplication of `m31`s in field.
    extern fn m31_mul(a: m31, b: m31) -> m31 nopanic;
    /// Division of `m31`s in field.
    extern fn m31_div(a: m31, b: NonZero<m31>) -> m31 nopanic;

    pub use {m31_add as add, m31_div as div, m31_mul as mul, m31_sub as sub};
}
