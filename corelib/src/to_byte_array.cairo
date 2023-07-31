use byte_array::ByteArrayTrait;
use traits::{Into, TryInto};
use option::OptionTrait;
use zeroable::Zeroable;

trait FormatAsByteArrayHex<T> {
    fn format_as_byte_array_hex(self: T) -> ByteArray;
}
trait FormatAsByteArrayDec<T> {
    fn format_as_byte_array_dec(self: T) -> ByteArray;
}
trait FormatAsByteArrayOct<T> {
    fn format_as_byte_array_oct(self: T) -> ByteArray;
}
trait FormatAsByteArrayBin<T> {
    fn format_as_byte_array_bin(self: T) -> ByteArray;
}

// === Hexadecimal base ===

impl U8FormatAsByteArrayHex of FormatAsByteArrayHex<u8> {
    fn format_as_byte_array_hex(self: u8) -> ByteArray {
        to_byte_array_hex(self, 16_u8.try_into().unwrap())
    }
}
impl U16FormatAsByteArrayHex of FormatAsByteArrayHex<u16> {
    fn format_as_byte_array_hex(self: u16) -> ByteArray {
        to_byte_array_hex(self, 16_u16.try_into().unwrap())
    }
}
impl U32FormatAsByteArrayHex of FormatAsByteArrayHex<u32> {
    fn format_as_byte_array_hex(self: u32) -> ByteArray {
        to_byte_array_hex(self, 16_u32.try_into().unwrap())
    }
}
impl U64FormatAsByteArrayHex of FormatAsByteArrayHex<u64> {
    fn format_as_byte_array_hex(self: u64) -> ByteArray {
        to_byte_array_hex(self, 16_u64.try_into().unwrap())
    }
}
impl U128FormatAsByteArrayHex of FormatAsByteArrayHex<u128> {
    fn format_as_byte_array_hex(self: u128) -> ByteArray {
        to_byte_array_hex(self, 16_u128.try_into().unwrap())
    }
}
impl U256FormatAsByteArrayHex of FormatAsByteArrayHex<u256> {
    fn format_as_byte_array_hex(self: u256) -> ByteArray {
        to_byte_array_hex(self, 16_u256.try_into().unwrap())
    }
}
impl Felt252FormatAsByteArrayHex of FormatAsByteArrayHex<felt252> {
    fn format_as_byte_array_hex(self: felt252) -> ByteArray {
        let as_u256: u256 = self.into();
        as_u256.format_as_byte_array_hex()
    }
}

// TODO(yuval): once const generic parameters are supported, move hex_base_nz to be a generic
// parameter. Same for the other bases.
/// Formats a type that behaves like uint into a hexadecimal string.
fn to_byte_array_hex<
    T,
    impl DropImpl: Drop<T>,
    impl CopyImpl: Copy<T>,
    impl DivRemImpl: DivRem<T>,
    impl TryIntoU8: TryInto<T, u8>,
    impl ZeroableImpl: Zeroable<T>,
>(
    mut value: T, hex_base_nz: NonZero<T>,
) -> ByteArray {
    let mut result: ByteArray = "";
    loop {
        let (new_value, digit) = DivRem::div_rem(value, hex_base_nz);
        value = new_value;
        let digit_as_u8: u8 = digit.try_into().unwrap();
        result
            .append_byte(if digit_as_u8 < 10 {
                digit_as_u8 + '0'
            } else {
                digit_as_u8 - 10 + 'A'
            });
        if value.is_zero() {
            break;
        };
    };
    "0x" + result.rev()
}

// === Decimal base ===

impl U8FormatAsByteArrayDec of FormatAsByteArrayDec<u8> {
    fn format_as_byte_array_dec(self: u8) -> ByteArray {
        to_byte_array_dec(self, 10_u8.try_into().unwrap())
    }
}
impl U16FormatAsByteArrayDec of FormatAsByteArrayDec<u16> {
    fn format_as_byte_array_dec(self: u16) -> ByteArray {
        to_byte_array_dec(self, 10_u16.try_into().unwrap())
    }
}
impl U32FormatAsByteArrayDec of FormatAsByteArrayDec<u32> {
    fn format_as_byte_array_dec(self: u32) -> ByteArray {
        to_byte_array_dec(self, 10_u32.try_into().unwrap())
    }
}
impl U64FormatAsByteArrayDec of FormatAsByteArrayDec<u64> {
    fn format_as_byte_array_dec(self: u64) -> ByteArray {
        to_byte_array_dec(self, 10_u64.try_into().unwrap())
    }
}
impl U128FormatAsByteArrayDec of FormatAsByteArrayDec<u128> {
    fn format_as_byte_array_dec(self: u128) -> ByteArray {
        to_byte_array_dec(self, 10_u128.try_into().unwrap())
    }
}
impl U256FormatAsByteArrayDec of FormatAsByteArrayDec<u256> {
    fn format_as_byte_array_dec(self: u256) -> ByteArray {
        to_byte_array_dec(self, 10_u256.try_into().unwrap())
    }
}
impl Felt252FormatAsByteArrayDec of FormatAsByteArrayDec<felt252> {
    fn format_as_byte_array_dec(self: felt252) -> ByteArray {
        let as_u256: u256 = self.into();
        as_u256.format_as_byte_array_dec()
    }
}

/// Formats a type that behaves like uint into a decimal string.
fn to_byte_array_dec<
    T,
    impl DropImpl: Drop<T>,
    impl CopyImpl: Copy<T>,
    impl DivRemImpl: DivRem<T>,
    impl TryIntoU8: TryInto<T, u8>,
    impl ZeroableImpl: Zeroable<T>,
>(
    mut value: T, dec_base_nz: NonZero<T>,
) -> ByteArray {
    let mut result: ByteArray = "";
    loop {
        let (new_value, digit) = DivRem::div_rem(value, dec_base_nz);
        value = new_value;
        let digit_as_u8: u8 = digit.try_into().unwrap();
        result.append_byte(digit_as_u8 + '0');
        if value.is_zero() {
            break;
        };
    };
    result.rev()
}

// === Octal base ===

impl U8FormatAsByteArrayOct of FormatAsByteArrayOct<u8> {
    fn format_as_byte_array_oct(self: u8) -> ByteArray {
        to_byte_array_oct(self, 8_u8.try_into().unwrap())
    }
}
impl U16FormatAsByteArrayOct of FormatAsByteArrayOct<u16> {
    fn format_as_byte_array_oct(self: u16) -> ByteArray {
        to_byte_array_oct(self, 8_u16.try_into().unwrap())
    }
}
impl U32FormatAsByteArrayOct of FormatAsByteArrayOct<u32> {
    fn format_as_byte_array_oct(self: u32) -> ByteArray {
        to_byte_array_oct(self, 8_u32.try_into().unwrap())
    }
}
impl U64FormatAsByteArrayOct of FormatAsByteArrayOct<u64> {
    fn format_as_byte_array_oct(self: u64) -> ByteArray {
        to_byte_array_oct(self, 8_u64.try_into().unwrap())
    }
}
impl U128FormatAsByteArrayOct of FormatAsByteArrayOct<u128> {
    fn format_as_byte_array_oct(self: u128) -> ByteArray {
        to_byte_array_oct(self, 8_u128.try_into().unwrap())
    }
}
impl U256FormatAsByteArrayOct of FormatAsByteArrayOct<u256> {
    fn format_as_byte_array_oct(self: u256) -> ByteArray {
        to_byte_array_oct(self, 8_u256.try_into().unwrap())
    }
}
impl Felt252FormatAsByteArrayOct of FormatAsByteArrayOct<felt252> {
    fn format_as_byte_array_oct(self: felt252) -> ByteArray {
        let as_u256: u256 = self.into();
        as_u256.format_as_byte_array_oct()
    }
}

/// Formats a type that behaves like uint into an octal string.
fn to_byte_array_oct<
    T,
    impl DropImpl: Drop<T>,
    impl CopyImpl: Copy<T>,
    impl DivRemImpl: DivRem<T>,
    impl TryIntoU8: TryInto<T, u8>,
    impl ZeroableImpl: Zeroable<T>,
>(
    mut value: T, dec_base_nz: NonZero<T>,
) -> ByteArray {
    let mut result: ByteArray = "";
    loop {
        let (new_value, digit) = DivRem::div_rem(value, dec_base_nz);
        value = new_value;
        let digit_as_u8: u8 = digit.try_into().unwrap();
        result.append_byte(digit_as_u8 + '0');
        if value.is_zero() {
            break;
        };
    };
    "0o" + result.rev()
}

// === Binary base ===

impl U8FormatAsByteArrayBin of FormatAsByteArrayBin<u8> {
    fn format_as_byte_array_bin(self: u8) -> ByteArray {
        to_byte_array_bin(self, 2_u8.try_into().unwrap())
    }
}
impl U16FormatAsByteArrayBin of FormatAsByteArrayBin<u16> {
    fn format_as_byte_array_bin(self: u16) -> ByteArray {
        to_byte_array_bin(self, 2_u16.try_into().unwrap())
    }
}
impl U32FormatAsByteArrayBin of FormatAsByteArrayBin<u32> {
    fn format_as_byte_array_bin(self: u32) -> ByteArray {
        to_byte_array_bin(self, 2_u32.try_into().unwrap())
    }
}
impl U64FormatAsByteArrayBin of FormatAsByteArrayBin<u64> {
    fn format_as_byte_array_bin(self: u64) -> ByteArray {
        to_byte_array_bin(self, 2_u64.try_into().unwrap())
    }
}
impl U128FormatAsByteArrayBin of FormatAsByteArrayBin<u128> {
    fn format_as_byte_array_bin(self: u128) -> ByteArray {
        to_byte_array_bin(self, 2_u128.try_into().unwrap())
    }
}
impl U256FormatAsByteArrayBin of FormatAsByteArrayBin<u256> {
    fn format_as_byte_array_bin(self: u256) -> ByteArray {
        to_byte_array_bin(self, 2_u256.try_into().unwrap())
    }
}
impl Felt252FormatAsByteArrayBin of FormatAsByteArrayBin<felt252> {
    fn format_as_byte_array_bin(self: felt252) -> ByteArray {
        let as_u256: u256 = self.into();
        as_u256.format_as_byte_array_bin()
    }
}

/// Formats a type that behaves like uint into a binary string.
fn to_byte_array_bin<
    T,
    impl DropImpl: Drop<T>,
    impl CopyImpl: Copy<T>,
    impl DivRemImpl: DivRem<T>,
    impl TryIntoU8: TryInto<T, u8>,
    impl ZeroableImpl: Zeroable<T>,
>(
    mut value: T, dec_base_nz: NonZero<T>,
) -> ByteArray {
    let mut result: ByteArray = "";
    loop {
        let (new_value, digit) = DivRem::div_rem(value, dec_base_nz);
        value = new_value;
        let digit_as_u8: u8 = digit.try_into().unwrap();
        result.append_byte(digit_as_u8 + '0');
        if value.is_zero() {
            break;
        };
    };
    "0b" + result.rev()
}
