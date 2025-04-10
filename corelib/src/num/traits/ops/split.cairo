//! Split a numeric value into halves for unsigned numeric types.

/// A trait for computing the split in bits of a number into halves.
///
/// # Examples
///
/// ```
/// use core::num::traits::Split;
///
/// assert!(0x908_u16.split() == (9, 8));
/// ```
pub trait Split<T> {
    /// The type of the result of the split operation.
    type Half;
    /// Computes the halving of the value.
    fn split(self: T) -> (Self::Half, Self::Half);
}

mod internal {
    #[feature("bounded-int-utils")]
    use crate::internal::bounded_int::{BoundedInt, DivRemHelper, UnitInt, div_rem, upcast};

    impl Helper<
        Full, const BOUND: felt252, const DIV: felt252,
    > of DivRemHelper<Full, UnitInt<DIV>> {
        type DivT = BoundedInt<0, BOUND>;
        type RemT = BoundedInt<0, BOUND>;
    }

    pub impl Impl<
        Full,
        Half,
        const BOUND: felt252,
        const DIV: felt252,
        const DIV_VALUE: NonZero<UnitInt<DIV>>,
    > of super::Split<Full> {
        type Half = Half;
        fn split(self: Full) -> (Half, Half) {
            let (high, low) = div_rem::<Full, _, Helper<Full, BOUND, DIV>>(self, DIV_VALUE);
            (upcast(high), upcast(low))
        }
    }
}

impl U16Split = internal::Impl<u16, u8, 0xff, 0x100, 0x100>;
impl U32Split = internal::Impl<u32, u16, 0xffff, 0x10000, 0x10000>;
impl U64Split = internal::Impl<u64, u32, 0xffffffff, 0x100000000, 0x100000000>;
impl U128Split =
    internal::Impl<u128, u64, 0xffffffffffffffff, 0x10000000000000000, 0x10000000000000000>;
