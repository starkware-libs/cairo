use traits::{Into, TryInto};
use option::OptionTrait;
use integer::{u256_from_felt252, u128_safe_divmod, u128_to_felt252};

const BYTES_IN_BYTES31: usize = 31;
const BYTES_IN_U128: usize = 16;
const POW_2_128: felt252 = 0x100000000000000000000000000000000;
const POW_2_8: u128 = 0x100;

#[derive(Copy, Drop)]
extern type bytes31;

extern fn bytes31_const<const value: felt252>() -> bytes31 nopanic;
extern fn bytes31_try_from_felt252(value: felt252) -> Option<bytes31> implicits(RangeCheck) nopanic;
extern fn bytes31_to_felt252(value: bytes31) -> felt252 nopanic;

#[generate_trait]
impl Bytes31Impl of Bytes31Trait {
    // Gets the byte at the given index (LSB's index is 0), assuming that
    // `index < BYTES_IN_BYTES31`. If the assumption is not met, the behavior is undefined.
    fn at(self: @bytes31, index: usize) -> u8 {
        let u256{low, high } = (*self).into();
        let res_u128 = if index < BYTES_IN_U128 {
            (low / one_shift_left_bytes_u128(index)) % POW_2_8
        } else {
            (high / one_shift_left_bytes_u128(index - BYTES_IN_U128)) % POW_2_8
        };
        res_u128.try_into().unwrap()
    }
}

impl Bytes31IndexView of IndexView<bytes31, usize, u8> {
    fn index(self: @bytes31, index: usize) -> u8 {
        self.at(index)
    }
}

impl Bytes31IntoFelt252 of Into<bytes31, felt252> {
    fn into(self: bytes31) -> felt252 {
        bytes31_to_felt252(self)
    }
}

impl Bytes31IntoU256 of Into<bytes31, u256> {
    fn into(self: bytes31) -> u256 {
        let as_felt: felt252 = self.into();
        as_felt.into()
    }
}

impl Felt252TryIntoBytes31 of TryInto<felt252, bytes31> {
    fn try_into(self: felt252) -> Option<bytes31> {
        bytes31_try_from_felt252(self)
    }
}

// TODO(yuval): implement all `into`s using `integer::upcast(self)`.
impl U8IntoBytes31 of Into<u8, bytes31> {
    fn into(self: u8) -> bytes31 {
        let as_felt: felt252 = self.into();
        as_felt.try_into().unwrap()
    }
}
impl U16IntoBytes31 of Into<u16, bytes31> {
    fn into(self: u16) -> bytes31 {
        let as_felt: felt252 = self.into();
        as_felt.try_into().unwrap()
    }
}
impl U32IntoBytes31 of Into<u32, bytes31> {
    fn into(self: u32) -> bytes31 {
        let as_felt: felt252 = self.into();
        as_felt.try_into().unwrap()
    }
}
impl U64IntoBytes31 of Into<u64, bytes31> {
    fn into(self: u64) -> bytes31 {
        let as_felt: felt252 = self.into();
        as_felt.try_into().unwrap()
    }
}
impl U128IntoBytes31 of Into<u128, bytes31> {
    fn into(self: u128) -> bytes31 {
        let as_felt: felt252 = self.into();
        as_felt.try_into().unwrap()
    }
}

// Splits a bytes31 into two bytes31s at the given index (LSB's index is 0).
// The bytes31s are represented using felt252s to improve performance.
// Note: this function assumes that:
// 1. `word` is validly convertible to a bytes31 which has no more than `len` bytes of data.
// 2. index <= len.
// 3. len <= BYTES_IN_BYTES31.
// If these assumptions are not met, it can corrupt the ByteArray. Thus, this should be a
// private function. We could add masking/assertions but it would be more expansive.
fn split_bytes31(word: felt252, len: usize, index: usize) -> (felt252, felt252) {
    if index == 0 {
        return (0, word);
    }
    if index == len {
        return (word, 0);
    }

    let u256{low, high } = word.into();

    if index == BYTES_IN_U128 {
        return (low.into(), high.into());
    }

    if len <= BYTES_IN_U128 {
        let (quotient, remainder) = u128_safe_divmod(
            low, one_shift_left_bytes_u128(index).try_into().unwrap()
        );
        return (remainder.into(), quotient.into());
    }

    // len > BYTES_IN_U128
    if index < BYTES_IN_U128 {
        let (low_quotient, low_remainder) = u128_safe_divmod(
            low, one_shift_left_bytes_u128(index).try_into().unwrap()
        );
        let right = high.into() * one_shift_left_bytes_u128(BYTES_IN_U128 - index).into()
            + low_quotient.into();
        return (low_remainder.into(), right);
    }

    // len > BYTES_IN_U128 && index > BYTES_IN_U128
    let (high_quotient, high_remainder) = u128_safe_divmod(
        high, one_shift_left_bytes_u128(index - BYTES_IN_U128).try_into().unwrap()
    );
    let left = high_remainder.into() * POW_2_128 + low.into();
    return (left, high_quotient.into());
}


// Returns 1 << (8 * `n_bytes`) as felt252, assuming that `n_bytes < BYTES_IN_BYTES31`.
//
// Note: if `n_bytes >= BYTES_IN_BYTES31`, the behavior is undefined. If one wants to assert that in
// the callsite, it's sufficient to assert that `n_bytes != BYTES_IN_BYTES31` because if
// `n_bytes > 31` then `n_bytes - 16 > 15` and `one_shift_left_bytes_u128` would panic.
fn one_shift_left_bytes_felt252(n_bytes: usize) -> felt252 {
    if n_bytes < BYTES_IN_U128 {
        one_shift_left_bytes_u128(n_bytes).into()
    } else {
        one_shift_left_bytes_u128(n_bytes - BYTES_IN_U128).into() * POW_2_128
    }
}

// Returns 1 << (8 * `n_bytes`) as u128, where `n_bytes` must be < BYTES_IN_U128.
//
// Panics if `n_bytes >= BYTES_IN_U128`.
fn one_shift_left_bytes_u128(n_bytes: usize) -> u128 {
    // TODO(yuval): change to match once it's supported for integers.
    if n_bytes == 0 {
        0x1_u128
    } else if n_bytes == 1 {
        0x100_u128
    } else if n_bytes == 2 {
        0x10000_u128
    } else if n_bytes == 3 {
        0x1000000_u128
    } else if n_bytes == 4 {
        0x100000000_u128
    } else if n_bytes == 5 {
        0x10000000000_u128
    } else if n_bytes == 6 {
        0x1000000000000_u128
    } else if n_bytes == 7 {
        0x100000000000000_u128
    } else if n_bytes == 8 {
        0x10000000000000000_u128
    } else if n_bytes == 9 {
        0x1000000000000000000_u128
    } else if n_bytes == 10 {
        0x100000000000000000000_u128
    } else if n_bytes == 11 {
        0x10000000000000000000000_u128
    } else if n_bytes == 12 {
        0x1000000000000000000000000_u128
    } else if n_bytes == 13 {
        0x100000000000000000000000000_u128
    } else if n_bytes == 14 {
        0x10000000000000000000000000000_u128
    } else if n_bytes == 15 {
        0x1000000000000000000000000000000_u128
    } else {
        panic_with_felt252('n_bytes too big')
    }
}

impl Bytes31PartialEq of PartialEq<bytes31> {
    fn eq(lhs: @bytes31, rhs: @bytes31) -> bool {
        let lhs_as_felt252: felt252 = (*lhs).into();
        let rhs_as_felt252: felt252 = (*rhs).into();
        lhs_as_felt252 == rhs_as_felt252
    }
    fn ne(lhs: @bytes31, rhs: @bytes31) -> bool {
        !(lhs == rhs)
    }
}
