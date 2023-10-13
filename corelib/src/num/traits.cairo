// === Zero ===

trait Zero<T> {
    /// Returns the multiplicative identity element of Self, 0.
    fn zero() -> T;
    /// Returns whether self is equal to 0, the multiplicative identity element.
    fn is_zero(self: @T) -> bool;
    /// Returns whether self is not equal to 0, the multiplicative identity element.
    fn is_non_zero(self: @T) -> bool;
}

impl Felt252Zero of Zero<felt252> {
    #[inline(always)]
    fn zero() -> felt252 {
        0
    }

    #[inline(always)]
    fn is_zero(self: @felt252) -> bool {
        *self == Zero::zero()
    }

    #[inline(always)]
    fn is_non_zero(self: @felt252) -> bool {
        !self.is_zero()
    }
}


impl U8Zeroable of Zero<u8> {
    fn zero() -> u8 {
        0
    }
    #[inline(always)]
    fn is_zero(self: @u8) -> bool {
        *self == Zero::zero()
    }
    #[inline(always)]
    fn is_non_zero(self: @u8) -> bool {
        !self.is_zero()
    }
}

impl U16Zeroable of Zero<u16> {
    fn zero() -> u16 {
        0
    }
    #[inline(always)]
    fn is_zero(self: @u16) -> bool {
        *self == Zero::zero()
    }
    #[inline(always)]
    fn is_non_zero(self: @u16) -> bool {
        !self.is_zero()
    }
}

impl U32Zeroable of Zero<u32> {
    fn zero() -> u32 {
        0
    }
    #[inline(always)]
    fn is_zero(self: @u32) -> bool {
        *self == Zero::zero()
    }
    #[inline(always)]
    fn is_non_zero(self: @u32) -> bool {
        !self.is_zero()
    }
}

impl U64Zeroable of Zero<u64> {
    fn zero() -> u64 {
        0
    }
    #[inline(always)]
    fn is_zero(self: @u64) -> bool {
        *self == Zero::zero()
    }
    #[inline(always)]
    fn is_non_zero(self: @u64) -> bool {
        !self.is_zero()
    }
}

impl U128Zeroable of Zero<u128> {
    fn zero() -> u128 {
        0
    }
    #[inline(always)]
    fn is_zero(self: @u128) -> bool {
        *self == Zero::zero()
    }
    #[inline(always)]
    fn is_non_zero(self: @u128) -> bool {
        !self.is_zero()
    }
}

impl U256Zeroable of Zero<u256> {
    fn zero() -> u256 {
        0
    }
    #[inline(always)]
    fn is_zero(self: @u256) -> bool {
        *self == Zero::zero()
    }
    #[inline(always)]
    fn is_non_zero(self: @u256) -> bool {
        !self.is_zero()
    }
}

trait One<T> {
    /// Returns the multiplicative identity element of Self, 1.
    fn one() -> T;
    /// Returns whether self is equal to 1, the multiplicative identity element.
    fn is_one(self: @T) -> bool;
    /// Returns whether self is not equal to 1, the multiplicative identity element.
    fn is_non_one(self: @T) -> bool;
}

impl Felt252One of One<felt252> {
    #[inline(always)]
    fn one() -> felt252 {
        1
    }

    #[inline(always)]
    fn is_one(self: @felt252) -> bool {
        *self == One::one()
    }

    #[inline(always)]
    fn is_non_one(self: @felt252) -> bool {
        !self.is_one()
    }
}

impl U8One of One<u8> {
    #[inline(always)]
    fn one() -> u8 {
        1
    }
    #[inline(always)]
    fn is_one(self: @u8) -> bool {
        *self == One::one()
    }
    #[inline(always)]
    fn is_non_one(self: @u8) -> bool {
        !self.is_one()
    }
}

impl U16One of One<u16> {
    #[inline(always)]
    fn one() -> u16 {
        1
    }
    #[inline(always)]
    fn is_one(self: @u16) -> bool {
        *self == One::one()
    }
    #[inline(always)]
    fn is_non_one(self: @u16) -> bool {
        !self.is_one()
    }
}

impl U32One of One<u32> {
    #[inline(always)]
    fn one() -> u32 {
        1
    }
    #[inline(always)]
    fn is_one(self: @u32) -> bool {
        *self == One::one()
    }
    #[inline(always)]
    fn is_non_one(self: @u32) -> bool {
        !self.is_one()
    }
}

impl U64One of One<u64> {
    #[inline(always)]
    fn one() -> u64 {
        1
    }
    #[inline(always)]
    fn is_one(self: @u64) -> bool {
        *self == One::one()
    }
    #[inline(always)]
    fn is_non_one(self: @u64) -> bool {
        !self.is_one()
    }
}

impl U128One of One<u128> {
    #[inline(always)]
    fn one() -> u128 {
        1
    }
    #[inline(always)]
    fn is_one(self: @u128) -> bool {
        *self == One::one()
    }
    #[inline(always)]
    fn is_non_one(self: @u128) -> bool {
        !self.is_one()
    }
}

impl U256One of One<u256> {
    #[inline(always)]
    fn one() -> u256 {
        1
    }
    #[inline(always)]
    fn is_one(self: @u256) -> bool {
        *self == One::one()
    }
    #[inline(always)]
    fn is_non_one(self: @u256) -> bool {
        !self.is_one()
    }
}
