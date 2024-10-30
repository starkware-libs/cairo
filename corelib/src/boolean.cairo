//! `BoolTrait` implementation.
//!
//! # Examples
//!
//! You can return an option of value with [`BoolTrait::then_some`]
//! depending on whether the `bool` is `true` or `false`
//!
//! ```
//! let bool = true;
//! let result = bool.then_some(0_u8);
//! assert!(result == Option::Some(0));
//! ```

/// `BoolTrait` generic implementation.
/// Explicit import of `BoolTrait` is required with `use core::boolean::BoolTrait;`.
#[generate_trait]
pub impl BoolImpl<T, +Drop<T>> of BoolTrait<T> {
    /// Returns `Option::Some(t)` if the `bool` is `true`, `Option::None` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(false.then_some(0) == Option::None);
    /// assert!(true.then_some(0) == Option::Some(0));
    /// ```
    #[inline]
    fn then_some(self: bool, t: T) -> Option<T> nopanic {
        if self {
            Option::Some(t)
        } else {
            Option::None
        }
    }
}
