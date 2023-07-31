use byte_array::ByteArrayTrait;
use traits::{Into, TryInto};
use option::OptionTrait;
use zeroable::Zeroable;

trait FormatAsByteArray<T> {
    fn format_as_byte_array(self: T, base: NonZero<T>) -> ByteArray;
}

impl U8FormatAsByteArray of FormatAsByteArray<u8> {
    fn format_as_byte_array(self: u8, base: NonZero<u8>) -> ByteArray {
        to_byte_array(self, base)
    }
}
impl U16FormatAsByteArray of FormatAsByteArray<u16> {
    fn format_as_byte_array(self: u16, base: NonZero<u16>) -> ByteArray {
        to_byte_array(self, base)
    }
}
impl U32FormatAsByteArray of FormatAsByteArray<u32> {
    fn format_as_byte_array(self: u32, base: NonZero<u32>) -> ByteArray {
        to_byte_array(self, base)
    }
}
impl U64FormatAsByteArray of FormatAsByteArray<u64> {
    fn format_as_byte_array(self: u64, base: NonZero<u64>) -> ByteArray {
        to_byte_array(self, base)
    }
}
impl U128FormatAsByteArray of FormatAsByteArray<u128> {
    fn format_as_byte_array(self: u128, base: NonZero<u128>) -> ByteArray {
        to_byte_array(self, base)
    }
}
impl U256FormatAsByteArray of FormatAsByteArray<u256> {
    fn format_as_byte_array(self: u256, base: NonZero<u256>) -> ByteArray {
        to_byte_array(self, base)
    }
}
impl Felt252FormatAsByteArray of FormatAsByteArray<felt252> {
    fn format_as_byte_array(self: felt252, base: NonZero<felt252>) -> ByteArray {
        let self_as_u256: u256 = self.into();
        let base: felt252 = base.into();
        let base: u256 = base.into();
        let base: NonZero<u256> = base.try_into().unwrap();
        self_as_u256.format_as_byte_array(base)
    }
}

// TODO(yuval): once const generic parameters are supported, move base_nz to be a generic
// parameter. Same for the other bases.
// TODO(yuval): support signed integers.
/// Formats a type that behaves like uint into a hexadecimal string.
fn to_byte_array<
    T,
    impl DropImpl: Drop<T>,
    impl CopyImpl: Copy<T>,
    impl DivRemImpl: DivRem<T>,
    impl TryIntoU8: TryInto<T, u8>,
    impl ZeroableImpl: Zeroable<T>,
>(
    mut value: T, base_nz: NonZero<T>,
) -> ByteArray {
    let base: T = base_nz.into();
    let base: u8 = base.try_into().unwrap();
    assert(base <= 36, 'base must be <= 36');

    let mut result: ByteArray = "";
    if base <= 10 {
        loop {
            let (new_value, digit) = DivRem::div_rem(value, base_nz);
            value = new_value;
            let digit_as_u8: u8 = digit.try_into().unwrap();
            result.append_byte(digit_as_u8 + '0');
            if value.is_zero() {
                break;
            };
        }
    } else {
        loop {
            let (new_value, digit) = DivRem::div_rem(value, base_nz);
            value = new_value;
            let digit_as_u8: u8 = digit.try_into().unwrap();
            result.append_byte(get_big_base_digit_representation(:digit_as_u8));
            if value.is_zero() {
                break;
            };
        };
    }

    let result = result.rev();
    if base == 16 {
        "0x" + result
    } else if base == 8 {
        "0o" + result
    } else if base == 2 {
        "0b" + result
    } else {
        result
    }
}

/// Converts a digit (0-9, A-Z) to its Ascii representation in a base > 10.
fn get_big_base_digit_representation(digit_as_u8: u8) -> u8 {
    if digit_as_u8 < 10 {
        digit_as_u8 + '0'
    } else {
        digit_as_u8 - 10 + 'A'
    }
}
