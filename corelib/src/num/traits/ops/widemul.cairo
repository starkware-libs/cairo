/// A trait for types that can be multiplied together to produce a wider type.
pub trait WideMul<Lhs, Rhs> {
    /// The type of the result of the multiplication.
    type Target;
    /// Multiply two values together, producing a wider type.
    fn wide_mul(lhs: Lhs, rhs: Rhs) -> Self::Target;
}

impl WideMulI8 of WideMul<i8, i8> {
    type Target = i16;
    fn wide_mul(lhs: i8, rhs: i8) -> i16 {
        core::integer::i8_wide_mul(lhs, rhs)
    }
}

impl WideMulI16 of WideMul<i16, i16> {
    type Target = i32;
    fn wide_mul(lhs: i16, rhs: i16) -> i32 {
        core::integer::i16_wide_mul(lhs, rhs)
    }
}

impl WideMulI32 of WideMul<i32, i32> {
    type Target = i64;
    fn wide_mul(lhs: i32, rhs: i32) -> i64 {
        core::integer::i32_wide_mul(lhs, rhs)
    }
}

impl WideMulI64 of WideMul<i64, i64> {
    type Target = i128;
    fn wide_mul(lhs: i64, rhs: i64) -> i128 {
        core::integer::i64_wide_mul(lhs, rhs)
    }
}

impl WideMulU8 of WideMul<u8, u8> {
    type Target = u16;
    fn wide_mul(lhs: u8, rhs: u8) -> u16 {
        core::integer::u8_wide_mul(lhs, rhs)
    }
}

impl WideMulU16 of WideMul<u16, u16> {
    type Target = u32;
    fn wide_mul(lhs: u16, rhs: u16) -> u32 {
        core::integer::u16_wide_mul(lhs, rhs)
    }
}

impl WideMulU32 of WideMul<u32, u32> {
    type Target = u64;
    fn wide_mul(lhs: u32, rhs: u32) -> u64 {
        core::integer::u32_wide_mul(lhs, rhs)
    }
}

impl WideMulU64 of WideMul<u64, u64> {
    type Target = u128;
    fn wide_mul(lhs: u64, rhs: u64) -> u128 {
        core::integer::u64_wide_mul(lhs, rhs)
    }
}

impl WideMulU128 of WideMul<u128, u128> {
    type Target = u256;
    fn wide_mul(lhs: u128, rhs: u128) -> u256 {
        let (high, low) = core::integer::u128_wide_mul(lhs, rhs);
        u256 { low, high }
    }
}

impl WideMulU256 of WideMul<u256, u256> {
    type Target = core::integer::u512;
    fn wide_mul(lhs: u256, rhs: u256) -> core::integer::u512 {
        core::integer::u256_wide_mul(lhs, rhs)
    }
}
