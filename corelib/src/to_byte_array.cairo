//! ASCII representation of numeric types for `ByteArray` manipulation.
//!
//! This module enables conversion of numeric values into their ASCII string representation,
//! with support for different numeric bases and efficient appending to existing `ByteArray`.
//!
//! # Examples
//!
//! Basic decimal formatting:
//!
//! ```
//! use core::to_byte_array::{FormatAsByteArray, AppendFormattedToByteArray};
//!
//! let value: u32 = 42;
//! let base: NonZero<u32> = 10;
//!
//! // Create a new formatted `ByteArray`
//! let formatted = value.format_as_byte_array(base);
//! assert!(formatted == "42");
//!
//! // Append to an existing `ByteArray`
//! let mut buffer = "Value: ";
//! value.append_formatted_to_byte_array(ref buffer, base);
//! assert!(buffer == "Value: 42");
//! ```
//!
//! Custom base formatting:
//!
//! ```
//! use core::to_byte_array::FormatAsByteArray;
//! let value: u32 = 255;
//!
//! // Hexadecimal representation
//! let hex = value.format_as_byte_array(16);
//! assert!(hex == "ff");
//!
//! // Binary representation
//! let bin = value.format_as_byte_array(2);
//! assert!(bin == "11111111");
//! ```

use crate::byte_array::ByteArrayTrait;
use crate::option::OptionTrait;
use crate::traits::{Into, TryInto};
use crate::zeroable::Zeroable;

/// A trait for appending the ASCII representation of a number to an existing `ByteArray`.
pub trait AppendFormattedToByteArray<T> {
    /// Appends the ASCII representation of the value to the provided `ByteArray`.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::to_byte_array::AppendFormattedToByteArray;
    ///
    /// let mut buffer = "Count: ";
    /// let num: u32 = 42;
    /// num.append_formatted_to_byte_array(ref buffer, 10);
    /// assert!(buffer == "Count: 42");
    /// ```
    fn append_formatted_to_byte_array(self: @T, ref byte_array: ByteArray, base: NonZero<T>);
}

/// A trait for formatting values into their ASCII string representation in a `ByteArray`.
pub trait FormatAsByteArray<T> {
    /// Returns a new `ByteArray` containing the ASCII representation of the value.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::to_byte_array::FormatAsByteArray;
    ///
    /// let num: u32 = 42;
    /// let formatted = num.format_as_byte_array(16);
    /// assert!(formatted, "2a");
    /// ```
    fn format_as_byte_array(self: @T, base: NonZero<T>) -> ByteArray;
}

impl FormatAsByteArrayImpl<T, +AppendFormattedToByteArray<T>> of FormatAsByteArray<T> {
    fn format_as_byte_array(self: @T, base: NonZero<T>) -> ByteArray {
        let mut byte_array = "";
        self.append_formatted_to_byte_array(ref byte_array, base);
        byte_array
    }
}

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
        self: @felt252, ref byte_array: ByteArray, base: NonZero<felt252>,
    ) {
        let self_as_u256: u256 = (*self).into();
        let base: felt252 = base.into();
        let base: u256 = base.into();
        let base: NonZero<u256> = base.try_into().unwrap();
        self_as_u256.append_formatted_to_byte_array(ref byte_array, base);
    }
}

// TODO(yuval): once const generic parameters are supported, move base_nz to be a generic
// parameter. Same for the other bases.
// TODO(yuval): support signed integers.
//
// Formats a type that behaves like uint to its ASCII representation and appends the formatted
// result into the given `ByteArray`.
// `base` must be in the range [2, 36]. Otherwise, this function panics.
fn append_formatted_to_byte_array<T, +Drop<T>, +Copy<T>, +DivRem<T>, +TryInto<T, u8>, +Zeroable<T>>(
    mut value: @T, ref byte_array: ByteArray, base_nz: NonZero<T>,
) {
    let base: T = base_nz.into();
    let base: u8 = base.try_into().unwrap();
    assert(base > 1, 'base must be > 1');
    assert(base <= 36, 'base must be <= 36');

    let mut reversed_digits = array![];

    if base <= 10 {
        loop {
            let (new_value, digit) = DivRem::div_rem(*value, base_nz);
            value = @new_value;
            let digit_as_u8: u8 = digit.try_into().unwrap();
            reversed_digits.append(digit_as_u8 + '0');
            if (*value).is_zero() {
                break;
            }
        }
    } else {
        loop {
            let (new_value, digit) = DivRem::div_rem(*value, base_nz);
            value = @new_value;
            let digit_as_u8: u8 = digit.try_into().unwrap();
            reversed_digits.append(get_big_base_digit_representation(:digit_as_u8));
            if (*value).is_zero() {
                break;
            }
        }
    }

    let mut span = reversed_digits.span();
    loop {
        match span.pop_back() {
            Some(byte) => { byte_array.append_byte(*byte); },
            None => { break; },
        }
    }
}

// Converts a digit (0-9, A-Z) to its ASCII representation in a base > 10.
#[inline]
fn get_big_base_digit_representation(digit_as_u8: u8) -> u8 {
    if digit_as_u8 < 10 {
        digit_as_u8 + '0'
    } else {
        digit_as_u8 - 10 + 'a'
    }
}
