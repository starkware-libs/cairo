//! Core panic mechanism.
//!
//! This module provides the core panic functionality used for error handling in Cairo.
//! It defines the basic types and functions used to trigger and manage panics, which
//! are Cairo's mechanism for handling unrecoverable errors.
//!
//! Panics can be triggered in several ways:
//!
//! Using the `panic` function:
//!
//! ```
//! use core::panics::panic;
//!
//! panic(array!['An error occurred']);
//! ```
//!
//! Or using the `panic!` macro:
//!
//! ```
//! panic!("Panic message");
//! ```
//!
//! This macro internally converts the message into a `ByteArray` and uses `panic_with_byte_array`.

use crate::array::Array;

/// Represents a panic condition in Cairo.
///
/// A `Panic` is created when the program encounters an unrecoverable error condition
/// and needs to terminate execution.
pub struct Panic {}

/// Result type for operations that can trigger a panic.
pub enum PanicResult<T> {
    Ok: T,
    Err: (Panic, Array<felt252>),
}

/// Triggers an immediate panic with the provided data and terminates execution.
///
/// # Examples
///
/// ```
/// use core::panics::panic;
///
/// panic(array!['An error occurred']);
/// ```
pub extern fn panic(data: Array<felt252>) -> crate::never;

/// Panics with a `ByteArray` message.
///
/// Constructs a panic message by prepending the `BYTE_ARRAY_MAGIC` value and
/// serializing the provided `ByteArray` into the panic data.
///
/// # Examples
///
/// ```
/// use core::panics::panic_with_byte_array;
///
/// let error_msg = "An error occurred";
/// panic_with_byte_array(@error_msg);
/// ```
#[inline(never)]
pub fn panic_with_byte_array(err: @ByteArray) -> crate::never {
    let mut serialized = array![crate::byte_array::BYTE_ARRAY_MAGIC];
    err.serialize(ref serialized);
    panic(serialized)
}
