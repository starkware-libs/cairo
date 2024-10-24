/// A trait for types that can be multiplied together to produce a wider type.
pub trait WideMul<Lhs, Rhs> {
    /// The type of the result of the multiplication.
    type Target;
    /// Multiply two values together, producing a wider type.
    fn wide_mul(self: Lhs, other: Rhs) -> Self::Target;
}

impl WideMulI8 of WideMul<i8, i8> {
    type Target = i16;
    fn wide_mul(self: i8, other: i8) -> i16 {
        crate::integer::i8_wide_mul(self, other)
    }
}

impl WideMulI16 of WideMul<i16, i16> {
    type Target = i32;
    fn wide_mul(self: i16, other: i16) -> i32 {
        crate::integer::i16_wide_mul(self, other)
    }
}

impl WideMulI32 of WideMul<i32, i32> {
    type Target = i64;
    fn wide_mul(self: i32, other: i32) -> i64 {
        crate::integer::i32_wide_mul(self, other)
    }
}

impl WideMulI64 of WideMul<i64, i64> {
    type Target = i128;
    fn wide_mul(self: i64, other: i64) -> i128 {
        crate::integer::i64_wide_mul(self, other)
    }
}

impl WideMulU8 of WideMul<u8, u8> {
    type Target = u16;
    fn wide_mul(self: u8, other: u8) -> u16 {
        crate::integer::u8_wide_mul(self, other)
    }
}

impl WideMulU16 of WideMul<u16, u16> {
    type Target = u32;
    fn wide_mul(self: u16, other: u16) -> u32 {
        crate::integer::u16_wide_mul(self, other)
    }
}

impl WideMulU32 of WideMul<u32, u32> {
    type Target = u64;
    fn wide_mul(self: u32, other: u32) -> u64 {
        crate::integer::u32_wide_mul(self, other)
    }
}

impl WideMulU64 of WideMul<u64, u64> {
    type Target = u128;
    fn wide_mul(self: u64, other: u64) -> u128 {
        crate::integer::u64_wide_mul(self, other)
    }
}

impl WideMulU128 of WideMul<u128, u128> {
    type Target = u256;
    fn wide_mul(self: u128, other: u128) -> u256 {
        let (high, low) = crate::integer::u128_wide_mul(self, other);
        u256 { low, high }
    }
}

impl WideMulU256 of WideMul<u256, u256> {
    type Target = crate::integer::u512;
    fn wide_mul(self: u256, other: u256) -> crate::integer::u512 {
        crate::integer::u256_wide_mul(self, other)
    }
}
