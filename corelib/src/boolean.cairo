//! Boolean operations.
//!
//! The `bool` type is a primitive type in Cairo representing a boolean value that can be either
//! `true` or `false`. This module provides trait implementations for boolean operations.
//!
//! # Examples
//!
//! Basic boolean operations:
//!
//! ```
//!
//! let value = true;
//! assert!(value == true);
//! assert!(!value == false);
//! ```
//!
//! Converting to optional values with [`BoolTrait::then_some`]:
//!
//! ```
//! use core::boolean::BoolTrait;
//!
//! let bool_value = true;
//! let result = bool_value.then_some(42_u8);
//! assert!(result == Some(42));
//!
//! let bool_value = false;
//! let result = bool_value.then_some(42_u8);
//! assert!(result == None);
//! ```

/// Basic trait for boolean operations.
/// Explicit import of `BoolTrait` is required with `use core::boolean::BoolTrait;`.
#[generate_trait]
pub impl BoolImpl<T, +Drop<T>> of BoolTrait<T> {
    /// Returns `Some(t)` if the `bool` is `true`, `None` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(false.then_some(0) == None);
    /// assert!(true.then_some(0) == Some(0));
    /// ```
    #[inline]
    fn then_some(self: bool, t: T) -> Option<T> nopanic {
        if self {
            Some(t)
        } else {
            None
        }
    }
}
