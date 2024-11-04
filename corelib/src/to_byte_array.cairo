//! ToByteArray module that provides traits and functions for formatting numeric types as their
//! ASCII representations and appending them to a `ByteArray`.
//!
//! The main purpose of this module is to provide a standardized way to convert numeric values
//! to their textual representations and append them to a `ByteArray` data structure, which is
//! useful for tasks like serialization, logging, and formatting output.
//!
//! # Examples
//!
//! ```
//! use core::to_byte_array::{FormatAsByteArray, AppendFormattedToByteArray};
//!
//! let value: u32 = 42;
//! let base: NonZero<u32> = 10;
//!
//! // Format the value as a ByteArray
//! let byte_array: ByteArray = value.format_as_byte_array(base);
//! assert!(byte_array == "42");
//!
//! // Append the formatted value to an existing ByteArray
//! let mut existing_array: ByteArray = "Hello, ";
//! value.append_formatted_to_byte_array(ref existing_array, base);
//! assert!(existing_array == "Hello, 42");
//! ```

use crate::byte_array::ByteArrayTrait;
use crate::option::OptionTrait;
use crate::traits::{Into, TryInto};
use crate::zeroable::Zeroable;

/// `AppendFormattedToByteArray` trait formats a type that behaves like `uint` to its ASCII
/// representation and appends the formatted result into the given `ByteArray`.
pub trait AppendFormattedToByteArray<T> {
    // Takes a snapshot of any type and a `NonZero` base value, and appends to the referenced
    // `ByteArray`
    // the formatted value.
    fn append_formatted_to_byte_array(self: @T, ref byte_array: ByteArray, base: NonZero<T>);
}

/// `FormatAsByteArray` trait formats the given input of a type that behaves like `uint` to its
/// ASCII representation in a `ByteArray`.
pub trait FormatAsByteArray<T> {
    // Takes a snapshot of any type and a `NonZero` base value, and returns a `ByteArray` containing
    // the formatted value.
    fn format_as_byte_array(self: @T, base: NonZero<T>) -> ByteArray;
}

/// `FormatAsByteArray` trait implementation that returns a `ByteArray` given a snapshot of a value
/// and the corresponding base.
impl FormatAsByteArrayImpl<T, +AppendFormattedToByteArray<T>> of FormatAsByteArray<T> {
    /// Returns a `ByteArray` given a snapshot of a value and the corresponding base.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::to_byte_array::FormatAsByteArray;
    ///
    /// let value: u32 = 42;
    /// let base: NonZero<u32> = 10;
    ///
    /// // Format the value as a ByteArray
    /// let byte_array: ByteArray = value.format_as_byte_array(base);
    /// assert!(byte_array == "42");
    /// ```
    fn format_as_byte_array(self: @T, base: NonZero<T>) -> ByteArray {
        let mut byte_array = "";
        self.append_formatted_to_byte_array(ref byte_array, :base);
        byte_array
    }
}

// === Impls of AppendFormattedToByteArray ===

/// `AppendFormattedToByteArray` trait implementation for `u8` that appends the formatted
/// result into the given `ByteArray`.
impl U8AppendFormattedToByteArray of AppendFormattedToByteArray<u8> {
    /// Formats a `u8` value to its ASCII representation and appends the formatted
    /// result into the given `ByteArray`.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::to_byte_array::AppendFormattedToByteArray;
    ///
    /// let value: u8 = 42;
    /// let base: NonZero<u8> = 10;
    /// let mut existing_array: ByteArray = "Hello, ";
    /// value.append_formatted_to_byte_array(ref existing_array, base);
    /// assert!(existing_array == "Hello, 42");
    /// ```
    fn append_formatted_to_byte_array(self: @u8, ref byte_array: ByteArray, base: NonZero<u8>) {
        append_formatted_to_byte_array(self, ref byte_array, base);
    }
}

/// `AppendFormattedToByteArray` trait implementation for `u16` that appends the formatted
/// result into the given `ByteArray`.
impl U16AppendFormattedToByteArray of AppendFormattedToByteArray<u16> {
    /// Formats a `u16` value to its ASCII representation and appends the formatted
    /// result into the given `ByteArray`.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::to_byte_array::AppendFormattedToByteArray;
    ///
    /// let value: u16 = 42;
    /// let base: NonZero<u16> = 10;
    /// let mut existing_array: ByteArray = "Hello, ";
    /// value.append_formatted_to_byte_array(ref existing_array, base);
    /// assert!(existing_array == "Hello, 42");
    /// ```
    fn append_formatted_to_byte_array(self: @u16, ref byte_array: ByteArray, base: NonZero<u16>) {
        append_formatted_to_byte_array(self, ref byte_array, base);
    }
}

/// `AppendFormattedToByteArray` trait implementation for `u32` that appends the formatted
/// result into the given `ByteArray`.
impl U32AppendFormattedToByteArray of AppendFormattedToByteArray<u32> {
    /// Formats a `u32` value to its ASCII representation and appends the formatted
    /// result into the given `ByteArray`.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::to_byte_array::AppendFormattedToByteArray;
    ///
    /// let value: u32 = 42;
    /// let base: NonZero<u32> = 10;
    /// let mut existing_array: ByteArray = "Hello, ";
    /// value.append_formatted_to_byte_array(ref existing_array, base);
    /// assert!(existing_array == "Hello, 42");
    /// ```
    fn append_formatted_to_byte_array(self: @u32, ref byte_array: ByteArray, base: NonZero<u32>) {
        append_formatted_to_byte_array(self, ref byte_array, base);
    }
}

/// `AppendFormattedToByteArray` trait implementation for `u64` that appends the formatted
/// result into the given `ByteArray`.
impl U64AppendFormattedToByteArray of AppendFormattedToByteArray<u64> {
    /// Formats a `u64` value to its ASCII representation and appends the formatted
    /// result into the given `ByteArray`.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::to_byte_array::AppendFormattedToByteArray;
    ///
    /// let value: u64 = 42;
    /// let base: NonZero<u64> = 10;
    /// let mut existing_array: ByteArray = "Hello, ";
    /// value.append_formatted_to_byte_array(ref existing_array, base);
    /// assert!(existing_array == "Hello, 42");
    /// ```
    fn append_formatted_to_byte_array(self: @u64, ref byte_array: ByteArray, base: NonZero<u64>) {
        append_formatted_to_byte_array(self, ref byte_array, base);
    }
}

/// `AppendFormattedToByteArray` trait implementation for `u128` that appends the formatted
/// result into the given `ByteArray`.
impl U128AppendFormattedToByteArray of AppendFormattedToByteArray<u128> {
    /// Formats a `u128` value to its ASCII representation and appends the formatted
    /// result into the given `ByteArray`.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::to_byte_array::AppendFormattedToByteArray;
    ///
    /// let value: u128 = 42;
    /// let base: NonZero<u128> = 10;
    /// let mut existing_array: ByteArray = "Hello, ";
    /// value.append_formatted_to_byte_array(ref existing_array, base);
    /// assert!(existing_array == "Hello, 42");
    /// ```
    fn append_formatted_to_byte_array(self: @u128, ref byte_array: ByteArray, base: NonZero<u128>) {
        append_formatted_to_byte_array(self, ref byte_array, base);
    }
}

/// `AppendFormattedToByteArray` trait implementation for `u256` that appends the formatted
/// result into the given `ByteArray`.
impl U256AppendFormattedToByteArray of AppendFormattedToByteArray<u256> {
    /// Formats a `u256` value to its ASCII representation and appends the formatted
    /// result into the given `ByteArray`.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::to_byte_array::AppendFormattedToByteArray;
    ///
    /// let value: u256 = 42;
    /// let base: NonZero<u256> = 10;
    /// let mut existing_array: ByteArray = "Hello, ";
    /// value.append_formatted_to_byte_array(ref existing_array, base);
    /// assert!(existing_array == "Hello, 42");
    /// ```
    fn append_formatted_to_byte_array(self: @u256, ref byte_array: ByteArray, base: NonZero<u256>) {
        append_formatted_to_byte_array(self, ref byte_array, base);
    }
}

/// `AppendFormattedToByteArray` trait implementation for `felt252` that appends the formatted
/// result into the given `ByteArray`.
impl Felt252AppendFormattedToByteArray of AppendFormattedToByteArray<felt252> {
    /// Formats a `felt252` value to its ASCII representation and appends the formatted
    /// result into the given `ByteArray`.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::to_byte_array::AppendFormattedToByteArray;
    ///
    /// let value = 42;
    /// let base: NonZero<felt252> = 10;
    /// let mut existing_array: ByteArray = "Hello, ";
    /// value.append_formatted_to_byte_array(ref existing_array, base);
    /// assert!(existing_array == "Hello, 42");
    /// ```
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
            };
        }
    } else {
        loop {
            let (new_value, digit) = DivRem::div_rem(*value, base_nz);
            value = @new_value;
            let digit_as_u8: u8 = digit.try_into().unwrap();
            reversed_digits.append(get_big_base_digit_representation(:digit_as_u8));
            if (*value).is_zero() {
                break;
            };
        };
    }

    let mut span = reversed_digits.span();
    loop {
        match span.pop_back() {
            Option::Some(byte) => { byte_array.append_byte(*byte); },
            Option::None => { break; },
        };
    };
}

// Converts a digit (0-9, A-Z) to its Ascii representation in a base > 10.
#[inline]
fn get_big_base_digit_representation(digit_as_u8: u8) -> u8 {
    if digit_as_u8 < 10 {
        digit_as_u8 + '0'
    } else {
        digit_as_u8 - 10 + 'a'
    }
}
