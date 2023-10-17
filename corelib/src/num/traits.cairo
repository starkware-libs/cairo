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


impl U8Zero of Zero<u8> {
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

impl U16Zero of Zero<u16> {
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

impl U32Zero of Zero<u32> {
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

impl U64Zero of Zero<u64> {
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

impl U128Zero of Zero<u128> {
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

impl U256Zero of Zero<u256> {
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

impl I8Zero of Zero<i8> {
    fn zero() -> i8 {
        0
    }
    #[inline(always)]
    fn is_zero(self: @i8) -> bool {
        *self == Zero::zero()
    }
    #[inline(always)]
    fn is_non_zero(self: @i8) -> bool {
        !self.is_zero()
    }
}

impl I16Zero of Zero<i16> {
    fn zero() -> i16 {
        0
    }
    #[inline(always)]
    fn is_zero(self: @i16) -> bool {
        *self == Zero::zero()
    }
    #[inline(always)]
    fn is_non_zero(self: @i16) -> bool {
        !self.is_zero()
    }
}

impl I32Zero of Zero<i32> {
    fn zero() -> i32 {
        0
    }
    #[inline(always)]
    fn is_zero(self: @i32) -> bool {
        *self == Zero::zero()
    }
    #[inline(always)]
    fn is_non_zero(self: @i32) -> bool {
        !self.is_zero()
    }
}

impl I64Zero of Zero<i64> {
    fn zero() -> i64 {
        0
    }
    #[inline(always)]
    fn is_zero(self: @i64) -> bool {
        *self == Zero::zero()
    }
    #[inline(always)]
    fn is_non_zero(self: @i64) -> bool {
        !self.is_zero()
    }
}

impl I128Zero of Zero<i128> {
    fn zero() -> i128 {
        0
    }
    #[inline(always)]
    fn is_zero(self: @i128) -> bool {
        *self == Zero::zero()
    }
    #[inline(always)]
    fn is_non_zero(self: @i128) -> bool {
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

impl I8One of One<i8> {
    fn one() -> i8 {
        1
    }

    #[inline(always)]
    fn is_one(self: @i8) -> bool {
        *self == One::one()
    }

    #[inline(always)]
    fn is_non_one(self: @i8) -> bool {
        !self.is_one()
    }
}

impl I16One of One<i16> {
    fn one() -> i16 {
        1
    }

    #[inline(always)]
    fn is_one(self: @i16) -> bool {
        *self == One::one()
    }

    #[inline(always)]
    fn is_non_one(self: @i16) -> bool {
        !self.is_one()
    }
}

impl I32One of One<i32> {
    fn one() -> i32 {
        1
    }

    #[inline(always)]
    fn is_one(self: @i32) -> bool {
        *self == One::one()
    }

    #[inline(always)]
    fn is_non_one(self: @i32) -> bool {
        !self.is_one()
    }
}

impl I64One of One<i64> {
    fn one() -> i64 {
        1
    }

    #[inline(always)]
    fn is_one(self: @i64) -> bool {
        *self == One::one()
    }

    #[inline(always)]
    fn is_non_one(self: @i64) -> bool {
        !self.is_one()
    }
}

impl I128One of One<i128> {
    fn one() -> i128 {
        1
    }

    #[inline(always)]
    fn is_one(self: @i128) -> bool {
        *self == One::one()
    }

    #[inline(always)]
    fn is_non_one(self: @i128) -> bool {
        !self.is_one()
    }
}
