//! Core panic mechanism for handling unrecoverable errors.
//!
//! This module implements a panic system that can carry error data and
//! supports both raw `felt252` arrays and structured byte array messages.
//!
//! The panic system uses a special magic value `BYTE_ARRAY_MAGIC` to distinguish
//! between raw panic data and structured byte array messages. When using
//! `panic_with_byte_array`, the error message is automatically serialized with this
//! magic value prepended.
//!
//! # Examples
//!
//! One can use the `panic` function to halt execution, using an array of `felt252` for the error
//! message:
//!
//! ```
//! panic(array![]);
//! ```
//!
//! ... but using `ByteArray` error message is generally more suitable:
//!
//! ```
//! use core::panics::panic_with_byte_array;
//!
//! panic_with_byte_array(@"");
//! ```
//!
//! Using the `panic!` macro that takes a `ByteArray` is even more straightforward:
//!
//! ```
//! panic!("");
//! ```

use crate::array::Array;

pub struct Panic {}

/// `PanicResult` enum that allows graceful handling of panics.
pub enum PanicResult<T> {
    Ok: T,
    Err: (Panic, Array<felt252>),
}

/// Panics with the given array of `felt252` as error message.
///
/// # Examples
///
/// ```
/// panic(array!['panic error message']);
/// ```
pub extern fn panic(data: Array<felt252>) -> crate::never;

/// Panics with the given `ByteArray` as error message. `panic_with_byte_array` ultimately calls
/// `panic` with `BYTE_ARRAY_MAGIC` appended to `array<felt252>` and then the serialized given
/// `ByteArray`.
///
/// # Examples
///
/// ```
/// use core::panics::panic_with_byte_array;
///
/// panic_with_byte_array(@"panic error message");
/// ```
#[inline]
pub fn panic_with_byte_array(err: @ByteArray) -> crate::never {
    let mut serialized = array![crate::byte_array::BYTE_ARRAY_MAGIC];
    err.serialize(ref serialized);
    panic(serialized)
}
