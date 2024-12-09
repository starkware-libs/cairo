//! The `Option<T>` type represents an optional value: every `Option<T>` is either `Some` and
//! contains a value, or `None`, and does not.
//!
//! Options are commonly paired with pattern matching to query the presence of a value and take
//! action, always accounting for the `None` case.
//!
//! When writing code that calls many functions that return the `Option` type, handling
//! `Some`/`None` can be tedious. The question mark operator `?` hides some of the boilerplate of
//! propagating values up the call stack.
//! Ending the expression with `?` will result in the `Some`s unwrapped value, unless the result is
//! `None`, in which case `None` is returned early from the enclosing function.

/// The `Option<T>` enum representing either `Some(value)` or `None`.
#[must_use]
#[derive(Copy, Drop, Debug, Serde, PartialEq)]
pub enum Option<T> {
    Some: T,
    None,
}

impl OptionDefault<T> of Default<Option<T>> {
    fn default() -> Option<T> {
        Option::None
    }
}

pub impl DestructOption<T, +Destruct<T>, -Drop<Option<T>>> of Destruct<Option<T>> {
    #[inline]
    fn destruct(self: Option<T>) nopanic {
        match self {
            Option::Some(x) => x.destruct(),
            Option::None => (),
        };
    }
}
/// A trait for handling `Option<T>` related operations.
pub trait OptionTrait<T> {
    /// Returns the contained `Some` value, consuming the `self` value.
    ///
    /// # Panics
    ///
    /// Panics if the option value is `None` with a custom `felt252` panic message `err`.
    ///
    /// # Examples
    ///
    /// ```
    /// let option = Option::Some(123);
    /// let value = option.expect('no value');
    /// assert!(value == 123);
    /// ```
    fn expect(self: Option<T>, err: felt252) -> T;

    /// Returns the contained `Some` value, consuming the `self` value.
    ///
    /// # Panics
    ///
    /// Panics if the `self` value equals `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// let option = Option::Some(123);
    /// let value = option.unwrap();
    /// assert!(value == 123);
    /// ```
    fn unwrap(self: Option<T>) -> T;

    /// Transforms the `Option<T>` into a `Result<T, E>`, mapping `Option::Some(v)` to
    /// `Result::Ok(v)` and `Option::None` to `Result::Err(err)`.
    ///
    /// # Examples
    ///
    /// ```
    /// let option = Option::Some(123);
    /// let result = option.ok_or('no value');
    /// assert!(result.unwrap() == 123);
    /// ```
    fn ok_or<E, +Destruct<E>>(self: Option<T>, err: E) -> Result<T, E>;

    /// Returns `true` if the `Option` is `Option::Some`, `false` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// let option = Option::Some(123);
    /// assert!(option.is_some());
    /// ```
    #[must_use]
    fn is_some(self: @Option<T>) -> bool;

    /// Returns `true` if the `Option` is `Option::None`, `false` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// let option = Option::Some(123);
    /// assert!(!option.is_none());
    /// ```
    #[must_use]
    fn is_none(self: @Option<T>) -> bool;

    /// Returns the contained `Some` value if `self` is `Option::Some(x)`. Otherwise, returns the
    /// provided default.
    ///
    /// # Examples
    ///
    /// ```
    /// let option = Option::Some(123);
    /// assert!(option.unwrap_or(456) == 123);
    ///
    /// let option = Option::None;
    /// assert!(option.unwrap_or(456) == 456);
    /// ```
    fn unwrap_or<+Destruct<T>>(self: Option<T>, default: T) -> T;

    /// Returns the contained `Some` value if `self` is `Option::Some(x)`. Otherwise, returns
    /// `Default::<T>::default()`.
    ///
    /// # Examples
    ///
    /// ```
    /// let option = Option::Some(123);
    /// assert!(option.unwrap_or_default() == 123);
    ///
    /// let option: Option<felt252> = Option::None;
    /// assert!(option.unwrap_or_default() == Default::default());
    /// ```
    fn unwrap_or_default<+Default<T>>(self: Option<T>) -> T;
}

pub impl OptionTraitImpl<T> of OptionTrait<T> {
    #[inline(always)]
    fn expect(self: Option<T>, err: felt252) -> T {
        match self {
            Option::Some(x) => x,
            Option::None => crate::panic_with_felt252(err),
        }
    }

    #[inline(always)]
    fn unwrap(self: Option<T>) -> T {
        self.expect('Option::unwrap failed.')
    }

    #[inline]
    fn ok_or<E, +Destruct<E>>(self: Option<T>, err: E) -> Result<T, E> {
        match self {
            Option::Some(v) => Result::Ok(v),
            Option::None => Result::Err(err),
        }
    }

    #[inline]
    fn is_some(self: @Option<T>) -> bool {
        match self {
            Option::Some(_) => true,
            Option::None => false,
        }
    }

    #[inline]
    fn is_none(self: @Option<T>) -> bool {
        match self {
            Option::Some(_) => false,
            Option::None => true,
        }
    }

    #[inline]
    fn unwrap_or<+Destruct<T>>(self: Option<T>, default: T) -> T {
        match self {
            Option::Some(x) => x,
            Option::None => default,
        }
    }

    #[inline]
    fn unwrap_or_default<+Default<T>>(self: Option<T>) -> T {
        match self {
            Option::Some(x) => x,
            Option::None => Default::default(),
        }
    }
}
