use core::num::traits::WideMul;

/// A trait for a type that can be squared to produce a wider type.
pub trait WidePow2<T> {
    /// The type of the result of the power of 2.
    type Target;
    /// Calculates the power of 2, producing a wider type.
    fn wide_pow2(self: T) -> Self::Target;
}

mod wide_mul_based {
    pub impl TWidePow2<T, impl TWideMul: super::WideMul<T, T>, +Copy<T>> of super::WidePow2<T> {
        type Target = TWideMul::Target;
        fn wide_pow2(self: T) -> Self::Target {
            TWideMul::wide_mul(self, self)
        }
    }
}

impl WidePow2I8 = wide_mul_based::TWidePow2<i8>;
impl WidePow2I16 = wide_mul_based::TWidePow2<i16>;
impl WidePow2I32 = wide_mul_based::TWidePow2<i32>;
impl WidePow2I64 = wide_mul_based::TWidePow2<i64>;
impl WidePow2U8 = wide_mul_based::TWidePow2<u8>;
impl WidePow2U16 = wide_mul_based::TWidePow2<u16>;
impl WidePow2U32 = wide_mul_based::TWidePow2<u32>;
impl WidePow2U64 = wide_mul_based::TWidePow2<u64>;
impl WidePow2U128 = wide_mul_based::TWidePow2<u128>;
impl WidePow2U256 of WidePow2<u256> {
    type Target = core::integer::u512;
    fn wide_pow2(self: u256) -> Self::Target {
        inner::u256_wide_pow2(self)
    }
}

mod inner {
    use core::integer::{u512, u128_add_with_bounded_int_carry, upcast};
    use core::internal::bounded_int;
    use core::num::traits::{WidePow2, WideMul, WrappingAdd};

    pub fn u256_wide_pow2(value: u256) -> u512 {
        let u256 { high: limb1, low: limb0 } = value.low.wide_pow2();
        let u256 { high: limb2, low: limb1_part } = value.low.wide_mul(value.high);
        let (limb1, limb1_overflow0) = u128_add_with_bounded_int_carry(limb1, limb1_part);
        let (limb1, limb1_overflow1) = u128_add_with_bounded_int_carry(limb1, limb1_part);
        let (limb2, limb2_overflow0) = u128_add_with_bounded_int_carry(limb2, limb2);
        let u256 { high: limb3, low: limb2_part } = value.high.wide_pow2();
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
