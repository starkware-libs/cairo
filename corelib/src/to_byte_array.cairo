use core::byte_array::ByteArrayTrait;
use core::traits::{Into, TryInto};
use core::option::OptionTrait;
use core::zeroable::Zeroable;

/// Formats a type that behaves like uint to its Ascii representation and appends the formatted
/// result into the given ByteArray.
pub trait AppendFormattedToByteArray<T> {
    fn append_formatted_to_byte_array(self: @T, ref byte_array: ByteArray, base: NonZero<T>);
}

/// Formats the given input of a type that behaves like uint to its Ascii representation in a
/// ByteArray.
pub trait FormatAsByteArray<T> {
    fn format_as_byte_array(self: @T, base: NonZero<T>) -> ByteArray;
}

impl FormatAsByteArrayImpl<T, +AppendFormattedToByteArray<T>> of FormatAsByteArray<T> {
    fn format_as_byte_array(self: @T, base: NonZero<T>) -> ByteArray {
        let mut byte_array = "";
        self.append_formatted_to_byte_array(ref byte_array, :base);
        byte_array
    }
}

// === Impls of AppendFormattedToByteArray ===

impl U8AppendFormattedToByteArray of AppendFormattedToByteArray<u8> {
    fn append_formatted_to_byte_array(self: @u8, ref byte_array: ByteArray, base: NonZero<u8>) {
        append_formatted_to_byte_array(self, ref byte_array, base);
    }
}
impl U16AppendFormattedToByteArray of AppendFormattedToByteArray<u16> {
    fn append_formatted_to_byte_array(self: @u16, ref byte_array: ByteArray, base: NonZero<u16>) {
        append_formatted_to_byte_array(self, ref byte_array, base);
    }
}
impl U32AppendFormattedToByteArray of AppendFormattedToByteArray<u32> {
    fn append_formatted_to_byte_array(self: @u32, ref byte_array: ByteArray, base: NonZero<u32>) {
        append_formatted_to_byte_array(self, ref byte_array, base);
    }
}
impl U64AppendFormattedToByteArray of AppendFormattedToByteArray<u64> {
    fn append_formatted_to_byte_array(self: @u64, ref byte_array: ByteArray, base: NonZero<u64>) {
        append_formatted_to_byte_array(self, ref byte_array, base);
    }
}
impl U128AppendFormattedToByteArray of AppendFormattedToByteArray<u128> {
    fn append_formatted_to_byte_array(self: @u128, ref byte_array: ByteArray, base: NonZero<u128>) {
        append_formatted_to_byte_array(self, ref byte_array, base);
    }
}
impl U256AppendFormattedToByteArray of AppendFormattedToByteArray<u256> {
    fn append_formatted_to_byte_array(self: @u256, ref byte_array: ByteArray, base: NonZero<u256>) {
        append_formatted_to_byte_array(self, ref byte_array, base);
    }
}
impl Felt252AppendFormattedToByteArray of AppendFormattedToByteArray<felt252> {
    fn append_formatted_to_byte_array(
        self: @felt252, ref byte_array: ByteArray, base: NonZero<felt252>
    ) {
        let self_as_u256: u256 = (*self).into();
        let base: felt252 = base.into();
        let base: u256 = base.into();
        let base: NonZero<u256> = base.try_into().unwrap();
        self_as_u256.append_formatted_to_byte_array(ref byte_array, base);
    }
}

// === helper functions ===

// TODO(yuval): once const generic parameters are supported, move base_nz to be a generic
// parameter. Same for the other bases.
// TODO(yuval): support signed integers.
/// Formats a type that behaves like uint to its Ascii representation and appends the formatted
/// result into the given ByteArray.
/// `base` must be in the range [2, 36]. Otherwise, this function panics.
fn append_formatted_to_byte_array<
    T, +Drop<T>, +Copy<T>, +DivRem<T>, +TryInto<T, u8>, +Zeroable<T>,
>(
    mut value: @T, ref byte_array: ByteArray, base_nz: NonZero<T>,
) {
    let base: T = base_nz.into();
    let base: u8 = base.try_into().unwrap();
    assert(base > 1, 'base must be > 1');

    if (*value).is_zero() {
        byte_array.append_byte('0');
        return;
    };

    if base <= 10 {
        append_small_digits_util(value, ref byte_array, base_nz);
    } else {
        assert(base <= 36, 'base must be <= 36');
        append_big_digits_util(value, ref byte_array, base_nz);
    }
}

/// Appends ascii representation of value in base_nz to byte_array for single digit base. 
fn append_small_digits_util<T, +Drop<T>, +Copy<T>, +DivRem<T>, +TryInto<T, u8>, +Zeroable<T>,>(
    mut value: @T, ref byte_array: ByteArray, base_nz: NonZero<T>,
) {
    let (new_value, digit) = DivRem::div_rem(*value, base_nz);
    let digit_as_u8: u8 = digit.try_into().unwrap();
    if !new_value.is_zero() {
        append_big_digits_util(@new_value, ref byte_array, base_nz);
    };
    byte_array.append_byte(digit_as_u8 + '0');
}

/// Appends ascii representation of value in base_nz to byte_array for any base.
fn append_big_digits_util<T, +Drop<T>, +Copy<T>, +DivRem<T>, +TryInto<T, u8>, +Zeroable<T>,>(
    mut value: @T, ref byte_array: ByteArray, base_nz: NonZero<T>,
) {
    let (new_value, digit) = DivRem::div_rem(*value, base_nz);
    let digit_as_u8: u8 = digit.try_into().unwrap();
    if !new_value.is_zero() {
        append_big_digits_util(@new_value, ref byte_array, base_nz);
    };
    byte_array.append_byte(get_big_base_digit_representation(:digit_as_u8));
}

/// Converts a digit (0-9, A-Z) to its Ascii representation in a base > 10.
#[inline]
fn get_big_base_digit_representation(digit_as_u8: u8) -> u8 {
    if digit_as_u8 < 10 {
        digit_as_u8 + '0'
    } else {
        digit_as_u8 - 10 + 'a'
    }
}
