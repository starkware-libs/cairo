use crate::num::traits::WideMul;

/// A trait for a type that can be squared to produce a wider type.
pub trait WideSquare<T> {
    /// The type of the result of the square.
    type Target;
    /// Calculates the square, producing a wider type.
    fn wide_square(self: T) -> Self::Target;
}

mod wide_mul_based {
    pub impl TWideSquare<T, impl TWideMul: super::WideMul<T, T>, +Copy<T>> of super::WideSquare<T> {
        type Target = TWideMul::Target;
        fn wide_square(self: T) -> Self::Target {
            TWideMul::wide_mul(self, self)
        }
    }
}

impl WideSquareI8 = wide_mul_based::TWideSquare<i8>;
impl WideSquareI16 = wide_mul_based::TWideSquare<i16>;
impl WideSquareI32 = wide_mul_based::TWideSquare<i32>;
impl WideSquareI64 = wide_mul_based::TWideSquare<i64>;
impl WideSquareU8 = wide_mul_based::TWideSquare<u8>;
impl WideSquareU16 = wide_mul_based::TWideSquare<u16>;
impl WideSquareU32 = wide_mul_based::TWideSquare<u32>;
impl WideSquareU64 = wide_mul_based::TWideSquare<u64>;
impl WideSquareU128 = wide_mul_based::TWideSquare<u128>;
impl WideSquareU256 of WideSquare<u256> {
    type Target = crate::integer::u512;
    fn wide_square(self: u256) -> Self::Target {
        inner::u256_wide_square(self)
    }
}

mod inner {
    use crate::integer::{u512, u128_add_with_bounded_int_carry, upcast};
    use crate::internal::bounded_int;
    use crate::num::traits::{WideSquare, WideMul, WrappingAdd};

    pub fn u256_wide_square(value: u256) -> u512 {
        let u256 { high: limb1, low: limb0 } = value.low.wide_square();
        let u256 { high: limb2, low: limb1_part } = value.low.wide_mul(value.high);
        let (limb1, limb1_overflow0) = u128_add_with_bounded_int_carry(limb1, limb1_part);
        let (limb1, limb1_overflow1) = u128_add_with_bounded_int_carry(limb1, limb1_part);
        let (limb2, limb2_overflow0) = u128_add_with_bounded_int_carry(limb2, limb2);
        let u256 { high: limb3, low: limb2_part } = value.high.wide_square();
        let (limb2, limb2_overflow1) = u128_add_with_bounded_int_carry(limb2, limb2_part);
        // Packing together the overflow bits, making a cheaper addition into limb2.
        let limb1_overflow = bounded_int::add(limb1_overflow0, limb1_overflow1);
        let (limb2, limb2_overflow2) = u128_add_with_bounded_int_carry(
            limb2, upcast(limb1_overflow)
        );
        // Packing together the overflow bits, making a cheaper addition into limb3.
        let limb2_overflow = bounded_int::add(limb2_overflow0, limb2_overflow1);
        let limb2_overflow = bounded_int::add(limb2_overflow, limb2_overflow2);
        // No overflow since no limb4.
        let limb3 = limb3.wrapping_add(upcast(limb2_overflow));
        u512 { limb0, limb1, limb2, limb3 }
    }
}
