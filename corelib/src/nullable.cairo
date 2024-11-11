//! Nullable module that provides a nullable wrapper type for handling optional values.
//!
//! `Nullable<T>` is a smart pointer type that can either point to a value or be null in the absence
//! of value.
//! The wrapped value is stored inside a `Box<T>` data type that uses the `boxed_segment` memory
//! segment to store data.
//!
//! This type is useful for avoiding null pointer dereferences. It is especially used in
//! dictionaries that store complex data structure that don't implement the `zero_default` method
//! of the `Felt252DictValue` trait which is called when a value does not exist in the dictionary.
//!
//! # Examples
//!
//! ```
//! use core::nullable::NullableTrait;
//!
//! // Create a nullable value
//! let value: Nullable<u32> = NullableTrait::new(10);
//!
//! // Dereference the value
//! let x = value.deref(); // Panics if the value is `Null`
//!
//! // Use the `deref_or` method to provide a default value
//! let y = value.deref_or(1);  // Returns 42 if value is not null, otherwise 1
//!
//! // Check if the value is null
//! if value.is_null() {
//!     // Handle null case
//! } else {
//!     // Handle non-null case
//! }
//! ```

use crate::box::BoxTrait;
use crate::traits::Default;
use crate::traits::Felt252DictValue;

/// A nullable wrapper type for handling optional values.
#[derive(Copy, Drop)]
pub extern type Nullable<T>;

/// `FromNullableResult` enum that stores either `null` or the boxed value.
pub enum FromNullableResult<T> {
    Null,
    NotNull: Box<T>,
}

pub extern fn null<T>() -> Nullable<T> nopanic;
pub(crate) extern fn nullable_from_box<T>(value: Box<T>) -> Nullable<T> nopanic;
pub extern fn match_nullable<T>(value: Nullable<T>) -> FromNullableResult<T> nopanic;
extern fn nullable_forward_snapshot<T>(value: @Nullable<T>) -> Nullable<@T> nopanic;

impl NullableDeref<T> of crate::ops::Deref<Nullable<T>> {
    type Target = T;
    /// `deref` implementation for `Nullable<T>` that returns the wrapped value.
    ///
    /// # Panics
    ///
    /// Panics if the wrapped value is `Null`.
    fn deref(self: Nullable<T>) -> T {
        match match_nullable(self) {
            FromNullableResult::Null => crate::panic_with_felt252('Attempted to deref null value'),
            FromNullableResult::NotNull(value) => value.unbox(),
        }
    }
}

/// `NullableTrait`containing basic methods to manipulate `Nullable<T>` smart pointers.
#[generate_trait]
pub impl NullableImpl<T> of NullableTrait<T> {
    /// Takes a value of type <T> and returns a `Nullable<T>` smart pointer.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::nullable::NullableTrait;
    ///
    /// let wrapped_value: Nullable<u32> = NullableTrait::new(10);
    /// ```
    #[must_use]
    fn new(value: T) -> Nullable<T> {
        nullable_from_box(BoxTrait::new(value))
    }

    /// Takes a `Nullable<T>` smart pointer and returns the wrapped value `T`.
    ///
    /// # Panics
    ///
    /// Panics if the value to be dereferenced is `Null`.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::nullable::NullableTrait;
    ///
    /// let wrapped_value: Nullable<u32> = NullableTrait::new(10);
    /// let value = wrapped_value.deref();
    /// ```
    fn deref(nullable: Nullable<T>) -> T {
        nullable.deref()
    }

    /// Takes a `Nullable<T>` smart pointer and a default value.
    /// Returns the wrapped value `T` if the value is not `Null`, the default value otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::nullable::NullableTrait;
    ///
    /// let wrapped_value: Nullable<u32> = NullableTrait::new(10);
    /// let value = wrapped_value.deref_or(0);
    /// ```
    fn deref_or<+Drop<T>>(self: Nullable<T>, default: T) -> T {
        match match_nullable(self) {
            FromNullableResult::Null => default,
            FromNullableResult::NotNull(value) => value.unbox(),
        }
    }

    /// Takes a snapshot of a `Nullable<T>` smart pointer and returns whether the wrapped value
    /// is null or not null.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::nullable::NullableTrait;
    ///
    /// let wrapped_value: Nullable<u32> = NullableTrait::new(10);
    /// let result = wrapped_value.is_null();
    /// ```
    #[must_use]
    fn is_null(self: @Nullable<T>) -> bool {
        match match_nullable(self.as_snapshot()) {
            FromNullableResult::Null => true,
            FromNullableResult::NotNull(_) => false,
        }
    }

    /// Takes a snapshot of a `Nullable<T>` smart pointer and returns a nullable smart pointer
    /// that points to a snapshot of the `T` value.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::nullable::NullableTrait;
    ///
    /// let wrapped_value: Nullable<u32> = NullableTrait::new(10);
    /// let snapshot = wrapped_value.as_snapshot();
    /// ```
    fn as_snapshot(self: @Nullable<T>) -> Nullable<@T> nopanic {
        nullable_forward_snapshot(self)
    }
}

impl NullableDefault<T> of Default<Nullable<T>> {
    /// Returns a `Nullable<T>` smart pointer with `Null` value.
    #[inline]
    #[must_use]
    fn default() -> Nullable<T> nopanic {
        null()
    }
}

impl NullableFelt252DictValue<T> of Felt252DictValue<Nullable<T>> {
    /// Returns a `Nullable<T>` smart pointer with `Null` value.
    #[inline]
    #[must_use]
    fn zero_default() -> Nullable<T> nopanic {
        null()
    }
}

impl NullableDebug<T, impl TDebug: crate::fmt::Debug<T>> of crate::fmt::Debug<Nullable<T>> {
    /// Formats a snapshot of a `Nullable<T>` smart pointer for printing and debugging purposes.
    fn fmt(self: @Nullable<T>, ref f: crate::fmt::Formatter) -> Result<(), crate::fmt::Error> {
        match match_nullable(self.as_snapshot()) {
            FromNullableResult::Null => write!(f, "null"),
            FromNullableResult::NotNull(value) => {
                write!(f, "&")?;
                TDebug::fmt(value.unbox(), ref f)
            },
        }
    }
}
