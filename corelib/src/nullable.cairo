//! A wrapper type for handling optional values.
//!
//! `Nullable<T>` is a wrapper type that can either contain a value stored in a `Box<T>`
//! or be null. It provides a safe way to handle optional values without the risk of
//! dereferencing null pointers.
//!
//! This makes it particularly useful in dictionaries that store complex data structures that don't
//! implement the `Felt252DictValue` trait; instead, they can be wrapped inside a `Nullable`.
//!
//! # Examples
//!
//! Basic usage:
//! ```
//! let value: Nullable<u32> = NullableTrait::new(10);
//! let unwrapped_value = value.deref();
//! ```
//!
//! Handling null values:
//! ```
//! let null_value: Nullable<u32> = Default::default();
//! let unwrapped_value = null_value.deref_or(1);
//! ```
//!
//! Checking if the value is null:
//! ```
//! let value: Nullable<u32> = NullableTrait::new(10);
//! let is_null = if value.is_null() {
//!     // Handle null case
//! } else {
//!     // Handle non-null case
//! };
//! ```

use crate::box::BoxTrait;
use crate::traits::{Default, Felt252DictValue};

/// A type that can either be null or contain a boxed value.
pub extern type Nullable<T>;

impl NullableCopy<T, +Copy<T>> of Copy<Nullable<T>>;
impl NullableDrop<T, +Drop<T>> of Drop<Nullable<T>>;

/// Represents the result of matching a `Nullable` value.
///
/// Used to safely handle both null and non-null cases when using `match_nullable` on a
/// `Nullable`.
pub enum FromNullableResult<T> {
    /// Represents a null value
    Null,
    /// The boxed value when not null
    NotNull: Box<T>,
}

pub extern fn null<T>() -> Nullable<T> nopanic;
pub(crate) extern fn nullable_from_box<T>(value: Box<T>) -> Nullable<T> nopanic;
pub extern fn match_nullable<T>(value: Nullable<T>) -> FromNullableResult<T> nopanic;
extern fn nullable_forward_snapshot<T>(value: @Nullable<T>) -> Nullable<@T> nopanic;

impl NullableDeref<T> of crate::ops::Deref<Nullable<T>> {
    type Target = T;
    /// Dereferences a `Nullable` to access its inner value.
    ///
    /// # Panics
    ///
    /// Panics if the value is null.
    ///
    /// # Examples
    ///
    /// ```
    /// let value: Nullable<u32> = NullableTrait::new(42);
    /// let unwrapped: u32 = value.deref();
    /// assert!(unwrapped == 42);
    ///
    /// let null_value: Nullable<u32> = Default::default();
    /// let will_panic = null_value.deref(); // Panics
    /// ```
    fn deref(self: Nullable<T>) -> T {
        match match_nullable(self) {
            FromNullableResult::Null => crate::panic_with_felt252('Attempted to deref null value'),
            FromNullableResult::NotNull(value) => value.unbox(),
        }
    }
}

/// Trait for creating and manipulating `Nullable` values.
#[generate_trait]
pub impl NullableImpl<T> of NullableTrait<T> {
    /// Wrapper for `Deref::deref`. Prefer using `Deref::deref` directly.
    ///
    /// This function exists for backwards compatibility.
    ///
    /// # Examples
    ///
    /// Preferred way:
    /// ```
    /// let value: Nullable<u32> = NullableTrait::new(42);
    /// let unwrapped = value.deref();
    /// ```
    ///
    /// This function method does the same thing:
    /// ```
    /// use core::nullable::NullableTrait;
    /// let also_unwrapped = NullableTrait::deref(value);
    /// ```
    fn deref(nullable: Nullable<T>) -> T {
        nullable.deref()
    }

    /// Returns the contained value if not null, or returns the provided default value.
    ///
    /// # Examples
    ///
    /// ```
    /// let value: Nullable<u32> = NullableTrait::new(42);
    /// assert!(value.deref_or(0) == 42);
    ///
    /// let null_value: Nullable<u32> = Default::default();
    /// assert!(null_value.deref_or(0) == 0);
    /// ```
    fn deref_or<+Drop<T>>(self: Nullable<T>, default: T) -> T {
        match match_nullable(self) {
            FromNullableResult::Null => default,
            FromNullableResult::NotNull(value) => value.unbox(),
        }
    }

    /// Creates a new non-null `Nullable` with the given value.
    ///
    /// # Examples
    ///
    /// ```
    /// let value: Nullable<u32> = NullableTrait::new(42);
    /// assert!(!value.is_null());
    /// ```
    #[must_use]
    fn new(value: T) -> Nullable<T> {
        nullable_from_box(BoxTrait::new(value))
    }

    /// Returns `true` if the value is null.
    ///
    /// # Examples
    ///
    /// ```
    /// let value: Nullable<u32> = NullableTrait::new(42);
    /// assert!(!value.is_null());
    ///
    /// let null_value: Nullable<u32> = Default::default();
    /// assert!(null_value.is_null());
    /// ```
    #[must_use]
    fn is_null(self: @Nullable<T>) -> bool {
        match match_nullable(self.as_snapshot()) {
            FromNullableResult::Null => true,
            FromNullableResult::NotNull(_) => false,
        }
    }

    /// Creates a new `Nullable` containing a snapshot of the value.
    ///
    /// This is useful when working with non-copyable types inside a `Nullable`.
    /// This allows you to keep using the original value while also having access to a
    /// snapshot of it, preventing the original value from being moved.
    ///
    /// # Examples
    ///
    /// ```
    /// let value: Nullable<Array<u32>> = NullableTrait::new(array![1, 2, 3]);
    /// let res = (@value).as_snapshot();
    /// assert!(res.deref() == @array![1, 2, 3]);
    /// assert!(value.deref() == array![1, 2, 3]);
    /// ```
    fn as_snapshot(self: @Nullable<T>) -> Nullable<@T> nopanic {
        nullable_forward_snapshot(self)
    }
}

impl NullableDefault<T> of Default<Nullable<T>> {
    /// Creates a new null value.
    ///
    /// # Examples
    ///
    /// ```
    /// let null_value: Nullable<u32> = Default::default();
    /// assert!(null_value.is_null());
    /// ```
    #[inline]
    #[must_use]
    fn default() -> Nullable<T> nopanic {
        null()
    }
}

impl NullableFelt252DictValue<T> of Felt252DictValue<Nullable<T>> {
    /// Creates a new null value for use in Felt252Dict.
    ///
    /// This implementation allows any type to be used as a value type in
    /// dictionaries when wrapped in a `Nullable`, where uninitialized entries
    /// return null.
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
