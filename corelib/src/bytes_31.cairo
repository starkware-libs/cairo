use traits::{Into, TryInto};
use option::OptionTrait;
use integer::{u256_from_felt252, u128_safe_divmod, u128_to_felt252};

const BYTES_IN_BYTES31: u8 = 31;
const BYTES_IN_U128: u8 = 16;

#[derive(Copy, Drop)]
extern type bytes31;

extern fn bytes31_const<const value: felt252>() -> bytes31 nopanic;
extern fn bytes31_try_from_felt252(value: felt252) -> Option<bytes31> implicits(RangeCheck) nopanic;
extern fn bytes31_to_felt252(value: bytes31) -> felt252 nopanic;

impl Bytes31IntoFelt252 of Into<bytes31, felt252> {
    fn into(self: bytes31) -> felt252 {
        bytes31_to_felt252(self)
    }
}

impl Felt252TryIntoBytes31 of TryInto<felt252, bytes31> {
    fn try_into(self: felt252) -> Option<bytes31> {
        bytes31_try_from_felt252(self)
    }
}

impl U8IntoBytes31 of Into<u8, bytes31> {
    fn into(self: u8) -> bytes31 {
        integer::upcast(self)
    }
}
impl U16IntoBytes31 of Into<u16, bytes31> {
    fn into(self: u16) -> bytes31 {
        integer::upcast(self)
    }
}
impl U32IntoBytes31 of Into<u32, bytes31> {
    fn into(self: u32) -> bytes31 {
        integer::upcast(self)
    }
}
impl U64IntoBytes31 of Into<u64, bytes31> {
    fn into(self: u64) -> bytes31 {
        integer::upcast(self)
    }
}
impl U128IntoBytes31 of Into<u128, bytes31> {
    fn into(self: u128) -> bytes31 {
        integer::upcast(self)
    }
}

#[generate_trait]
impl Bytes31Impl of Bytes31Trait {
    // Splits a bytes31 into two bytes31s at the given index.
    // Note: this function assumes that:
    // 1. `self` has no more than `len` bytes of data.
    // 2. index <= len.
    // 3. len <= BYTES_IN_BYTES31.
    // If these assumptions are not met, it can corrupt the ByteArray. Thus, this should be a
    // private function. We could add masking/assertions but it would be more expensive.
    fn split_bytes31(self: bytes31, len: u8, index: u8) -> (bytes31, bytes31) {
        if index == 0 {
            return (bytes31_const::<0>(), self);
        }
        if index == len {
            return (self, bytes31_const::<0>());
        }

        let u256{low, high } = u256_from_felt252(self.into());

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
            let right = u128_to_felt252(high) * one_shift_left_bytes_felt252(BYTES_IN_U128 - index)
                + u128_to_felt252(low_quotient);
            return (low_remainder.into(), right.try_into().unwrap());
        }

        // len > BYTES_IN_U128 && index > BYTES_IN_U128
        let (high_quotient, high_remainder) = u128_safe_divmod(
            high, one_shift_left_bytes_u128(index - BYTES_IN_U128).try_into().unwrap()
        );
        let left = u128_to_felt252(high_remainder) * POW_2_128 + u128_to_felt252(low);
        return (left.try_into().unwrap(), high_quotient.into());
    }
}

// Returns 1 << (8 * `n_bytes`) as felt252, where `n_bytes` must be < BYTES_IN_BYTES31.
fn one_shift_left_bytes_felt252(n_bytes: u8) -> felt252 {
    if n_bytes < BYTES_IN_U128 {
        one_shift_left_bytes_u128(n_bytes).into()
    } else {
        assert(n_bytes < BYTES_IN_BYTES31, 'n_bytes > 30');
        one_shift_left_bytes_u128(n_bytes - BYTES_IN_U128).into() * POW_2_128
    }
}

// Returns 1 << (8 * `n_bytes`) as u128, where `n_bytes` must be < BYTES_IN_U128.
fn one_shift_left_bytes_u128(n_bytes: u8) -> u128 {
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
        panic_with_felt252('n_bytes > 15')
    }
}
