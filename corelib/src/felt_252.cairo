pub(crate) impl Felt252Zero of core::num::traits::Zero<felt252> {
    fn zero() -> felt252 {
        0
    }

    #[inline(always)]
    fn is_zero(self: @felt252) -> bool {
        *self == Felt252Zero::zero()
    }

    #[inline(always)]
    fn is_non_zero(self: @felt252) -> bool {
        !self.is_zero()
    }
}

pub(crate) impl Felt252One of core::num::traits::One<felt252> {
    fn one() -> felt252 {
        1
    }

    #[inline(always)]
    fn is_one(self: @felt252) -> bool {
        *self == Felt252One::one()
    }

    #[inline(always)]
    fn is_non_one(self: @felt252) -> bool {
        !self.is_one()
    }
}
