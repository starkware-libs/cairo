use crate::zeroable::NonZero;

/// Performs truncated division **and** remainder.
///
/// `T` – dividend type (left-hand operand)
/// `U` – divisor  type (right-hand operand, must be wrapped in
///       [`NonZero<U>`](core::num::non_zero::NonZero) at call-site)
///
/// The division truncates toward zero, like Cairo’s `/` and `%`.
///
/// # Associated types
/// * [`Quotient`] – the type produced by the division
/// * [`Remainder`] – the type produced by the modulo
///
/// # Examples
///
/// Identical operand types:
/// ```cairo
/// use core::traits::{DivRem, NonZero};
///
/// let lhs: u32 = 7;
/// let rhs: NonZero<u32> = 3.try_into().unwrap();
/// assert!(DivRem::<u32, u32>::div_rem(lhs, rhs) == (2, 1));
/// ```
///
/// Heterogeneous division (`u256` by `u128`):
/// ```cairo
/// use core::traits::DivRem;
/// use integer::u256_as_non_zero;
///
/// let big: u256 = 1_000_000;                    // dividend
/// let nz10: NonZero<u128> = u256_as_non_zero(10_u128.into());  // divisor
///
/// let (q, r) = DivRem::<u256, u128>::div_rem(big, nz10);
/// // q : u256, r : u128
/// ```
pub trait DivRem<T, U> {
    /// Quotient returned by the division.
    type Quotient;

    /// Remainder returned by the modulo operation.
    type Remainder;

    /// Computes both `/` and `%` in a single pass.
    fn div_rem(lhs: T, rhs: NonZero<U>) -> (Self::Quotient, Self::Remainder);
}

//  Compatibility bridge:  DivRem<T>  →  DivRemGeneric<T,T>
mod _divrem_bridge {
    #[feature("generic-divrem")]
    use crate::traits::DivRem;
    use super::DivRem as DivRemGeneric;

    /// Generic adapter: if the old symmetric `DivRem<T>` exists,
    /// provide the corresponding `DivRemGeneric<T,T>` implementation.
    impl Bridge<T, +DivRem<T>> of DivRemGeneric<T, T> {
        type Quotient = T;
        type Remainder = T;

        fn div_rem(lhs: T, rhs: NonZero<T>) -> (T, T) {
            DivRem::<T>::div_rem(lhs, rhs)
        }
    }

    // Instantiate the generic adapter for every concrete integer type
    // that already has a symmetric `DivRem` implementation.
    pub impl BridgeU8 = Bridge<u8>;
    pub impl BridgeU16 = Bridge<u16>;
    pub impl BridgeU32 = Bridge<u32>;
    pub impl BridgeU64 = Bridge<u64>;
    pub impl BridgeU128 = Bridge<u128>;
    pub impl BridgeU256 = Bridge<u256>;
}
use _divrem_bridge::*;
