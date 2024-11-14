//! Utilities for the `felt252` type.
//!
//! The implementations defined in this module can be accessed by using the traits directly.

pub(crate) impl Felt252Zero of crate::num::traits::Zero<felt252> {
    fn zero() -> felt252 {
        0
    }

    #[inline]
    fn is_zero(self: @felt252) -> bool {
        *self == Self::zero()
    }

    #[inline]
    fn is_non_zero(self: @felt252) -> bool {
        !self.is_zero()
    }
}

pub(crate) impl Felt252One of crate::num::traits::One<felt252> {
    fn one() -> felt252 {
        1
    }

    #[inline]
    fn is_one(self: @felt252) -> bool {
        *self == Self::one()
    }

    #[inline]
    fn is_non_one(self: @felt252) -> bool {
        !self.is_one()
    }
}
