//! Definitions and utilities for the `bytes31` type.
//!
//! The `bytes31` type is a compact, indexable 31-byte type.
//!
//! # Examples
//!
//! Creating a `bytes31` from a `felt252`:
//! ```
//! let value: bytes31 = 0xaabb.try_into().unwrap();
//! ```
//!
//! Accessing a byte by index:
//! ```
//! assert!(value[0] == 0xbb);
//! ```

use crate::RangeCheck;
#[allow(unused_imports)]
use crate::integer::{u128_safe_divmod, u128_to_felt252};
use crate::num::traits::CheckedSub;
#[allow(unused_imports)]
use crate::option::OptionTrait;
use crate::traits::{Into, TryInto};

pub(crate) const BYTES_IN_BYTES31: usize = 31;
const BYTES_IN_U128: usize = 16;
pub(crate) const POW_2_128: felt252 = 0x100000000000000000000000000000000;
pub(crate) const POW_2_8: u128 = 0x100;

/// Represents a 31-byte fixed-size byte type.
pub extern type bytes31;

impl bytes31Copy of Copy<bytes31>;
impl bytes31Drop of Drop<bytes31>;

pub(crate) extern fn bytes31_const<const value: felt252>() -> bytes31 nopanic;
extern fn bytes31_try_from_felt252(value: felt252) -> Option<bytes31> implicits(RangeCheck) nopanic;
extern fn bytes31_to_felt252(value: bytes31) -> felt252 nopanic;

/// A trait for accessing a specific byte of a `bytes31` type.
#[generate_trait]
pub impl Bytes31Impl of Bytes31Trait {
    /// Returns the byte at the given index (LSB's index is 0).
    ///
    /// Assumes that `index < BYTES_IN_BYTES31`. If the assumption is not met, the behavior is
    /// undefined.
    ///
    /// # Examples
    ///
    /// ```
    /// let bytes: bytes31 = 1_u8.into();
    /// assert!(bytes.at(0) == 1);
    /// ```
    fn at(self: @bytes31, index: usize) -> u8 {
        u8_at_u256((*self).into(), index)
    }
}

#[feature("deprecated-index-traits")]
pub(crate) impl Bytes31IndexView of crate::traits::IndexView<bytes31, usize, u8> {
    fn index(self: @bytes31, index: usize) -> u8 {
        self.at(index)
    }
}

impl Bytes31BitSize of crate::num::traits::BitSize<bytes31> {
    fn bits() -> usize {
        248
    }
}

pub(crate) impl Bytes31IntoFelt252 of Into<bytes31, felt252> {
    fn into(self: bytes31) -> felt252 {
        bytes31_to_felt252(self)
    }
}

pub(crate) impl Bytes31IntoU256 of Into<bytes31, u256> {
    fn into(self: bytes31) -> u256 {
        let as_felt: felt252 = self.into();
        as_felt.into()
    }
}

pub(crate) impl Felt252TryIntoBytes31 of TryInto<felt252, bytes31> {
    fn try_into(self: felt252) -> Option<bytes31> {
        bytes31_try_from_felt252(self)
    }
}

impl Bytes31Serde = crate::serde::into_felt252_based::SerdeImpl<bytes31>;

pub(crate) impl U8IntoBytes31 of Into<u8, bytes31> {
    #[feature("bounded-int-utils")]
    const fn into(self: u8) -> bytes31 {
        crate::internal::bounded_int::upcast(self)
    }
}

impl U16IntoBytes31 of Into<u16, bytes31> {
    #[feature("bounded-int-utils")]
    const fn into(self: u16) -> bytes31 {
        crate::internal::bounded_int::upcast(self)
    }
}

impl U32IntoBytes31 of Into<u32, bytes31> {
    #[feature("bounded-int-utils")]
    const fn into(self: u32) -> bytes31 {
        crate::internal::bounded_int::upcast(self)
    }
}

impl U64IntoBytes31 of Into<u64, bytes31> {
    #[feature("bounded-int-utils")]
    const fn into(self: u64) -> bytes31 {
        crate::internal::bounded_int::upcast(self)
    }
}

pub(crate) impl U128IntoBytes31 of Into<u128, bytes31> {
    #[feature("bounded-int-utils")]
    const fn into(self: u128) -> bytes31 {
        crate::internal::bounded_int::upcast(self)
    }
}

/// Returns `1 << (8 * n_bytes)` as `u128`, where `n_bytes` must be < `BYTES_IN_U128`.
///
/// Panics if `n_bytes >= BYTES_IN_U128`.
pub(crate) fn one_shift_left_bytes_u128(n_bytes: usize) -> u128 {
    one_shift_left_bytes_u128_nz(n_bytes).into()
}

/// Splits a u128 into two words as a `u256` - the `low` is the first `n_bytes` the `high` is the
/// rest.
pub(crate) fn split_u128(value: u128, n_bytes: usize) -> u256 {
    let (high, low) = DivRem::div_rem(value, one_shift_left_bytes_u128_nz(n_bytes));
    u256 { low, high }
}

/// Returns the `u8` at `index` if you look at `value` as an array of 32 `u8`s.
pub(crate) fn u8_at_u256(value: u256, index: usize) -> u8 {
    get_lsb(
        if let Some(rev_index) = index.checked_sub(BYTES_IN_U128) {
            split_u128(value.high, rev_index).high
        } else {
            split_u128(value.low, index).high
        },
    )
}

/// Same as `one_shift_left_bytes_u128` but returns `NonZero` value.
fn one_shift_left_bytes_u128_nz(n_bytes: usize) -> NonZero<u128> {
    match n_bytes {
        0 => 0x1,
        1 => 0x100,
        2 => 0x10000,
        3 => 0x1000000,
        4 => 0x100000000,
        5 => 0x10000000000,
        6 => 0x1000000000000,
        7 => 0x100000000000000,
        8 => 0x10000000000000000,
        9 => 0x1000000000000000000,
        10 => 0x100000000000000000000,
        11 => 0x10000000000000000000000,
        12 => 0x1000000000000000000000000,
        13 => 0x100000000000000000000000000,
        14 => 0x10000000000000000000000000000,
        15 => 0x1000000000000000000000000000000,
        _ => crate::panic_with_felt252('n_bytes too big'),
    }
}

impl Bytes31PartialEq of PartialEq<bytes31> {
    fn eq(lhs: @bytes31, rhs: @bytes31) -> bool {
        let lhs_as_felt252: felt252 = (*lhs).into();
        let rhs_as_felt252: felt252 = (*rhs).into();
        lhs_as_felt252 == rhs_as_felt252
    }
}

mod helpers {
    #[feature("bounded-int-utils")]
    use core::internal::bounded_int::{BoundedInt, DivRemHelper, UnitInt, div_rem, upcast};

    impl DivRemU128By256 of DivRemHelper<u128, UnitInt<256>> {
        type DivT = BoundedInt<0, 0xffffffffffffffffffffffffffffff>;
        type RemT = BoundedInt<0, 0xff>;
    }

    /// Returns the least significant byte of the given u128.
    pub fn get_lsb(value: u128) -> u8 {
        let (_, res) = div_rem::<_, UnitInt<256>>(value, 256);
        upcast(res)
    }
}

pub(crate) use helpers::get_lsb;
