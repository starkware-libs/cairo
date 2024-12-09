//! Error handling with the `Result<T, E>` type.
//!
//! Functions might return `Result` whenever errors are expected and recoverable.
//! Pattern matching on `Result` is clear and straightforward for simple cases, but `Result` comes
//! with some convenience methods that make working with it more succinct.
//!
//! When writing code that calls many functions that return the `Result` type, the error handling
//! can be tedious. The question mark operator `?`, hides some of the boilerplate of propagating
//! errors up the call stack.
//! Ending the expression with `?` will result in the `Ok`s unwrapped value, unless the result is
//! `Err`, in which case `Err` is returned early from the enclosing function.

#[allow(unused_imports)]
use crate::array::ArrayTrait;
#[allow(unused_imports)]
use crate::serde::Serde;
#[allow(unused_imports)]
use crate::array::SpanTrait;

/// The type used for returning and propagating errors. It is an enum with the variants `Ok: T`,
/// representing success and containing a value, and `Err: E`, representing error and containing an
/// error value.
#[must_use]
#[derive(Copy, Drop, Debug, Serde, PartialEq)]
pub enum Result<T, E> {
    Ok: T,
    Err: E,
}

/// A trait for handling `Result<T, E>` related operations.
#[generate_trait]
pub impl ResultTraitImpl<T, E> of ResultTrait<T, E> {
    /// Returns the contained `Ok` value, consuming the `self` value.
    ///
    /// # Panics
    ///
    /// Panics if the value is an `Err`, with the provided `felt252` panic message.
    ///
    /// # Examples
    ///
    /// ```
    /// let result: Result<felt252, felt252> = Result::Ok(123);
    /// assert!(result.expect('no value') == 123);
    /// ```
    fn expect<+PanicDestruct<E>>(self: Result<T, E>, err: felt252) -> T {
        match self {
            Result::Ok(x) => x,
            Result::Err(_) => crate::panic_with_felt252(err),
        }
    }

    /// Returns the contained `Ok` value, consuming the `self` value.
    ///
    /// # Panics
    ///
    /// Panics if the value is an `Err`, with a standard `Result::unwrap failed` panic message.
    ///
    /// # Examples
    ///
    /// ```
    /// let result: Result<felt252, felt252> = Result::Ok(123);
    /// assert!(result.unwrap() == 123);
    /// ```
    fn unwrap<+Destruct<E>>(self: Result<T, E>) -> T {
        self.expect('Result::unwrap failed.')
    }

    /// Returns the contained `Ok` value or a provided default.
    ///
    /// # Examples
    ///
    /// ```
    /// let result: Result<felt252, felt252> = Result::Ok(123);
    /// assert!(result.unwrap_or(456) == 123);
    ///
    /// let result: Result<felt252, felt252> = Result::Err('no value');
    /// assert!(result.unwrap_or(456) == 456);
    /// ```
    fn unwrap_or<+Destruct<T>, +Destruct<E>>(self: Result<T, E>, default: T) -> T {
        match self {
            Result::Ok(x) => x,
            Result::Err(_) => default,
        }
    }

    /// Returns the contained `Ok`` value or `Default::<T>::default()`.
    ///
    /// # Examples
    ///
    /// ```
    /// let result: Result<felt252, felt252> = Result::Ok(123);
    /// assert!(result.unwrap_or_default() == 123);
    ///
    /// let result: Result<felt252, felt252> = Result::Err('no value');
    /// assert!(result.unwrap_or_default() == 0);
    /// ```
    fn unwrap_or_default<+Destruct<E>, +Default<T>>(self: Result<T, E>) -> T {
        match self {
            Result::Ok(x) => x,
            Result::Err(_) => Default::default(),
        }
    }

    /// Returns the contained `Err` value, consuming the `self` value.
    ///
    /// # Panics
    ///
    /// Panics if the value is an `Ok`, with the provided `felt252` panic message.
    ///
    /// # Examples
    ///
    /// ```
    /// let result: Result<felt252, felt252> = Result::Err('no value');
    /// assert!(result.expect_err('result is ok') == 'no value');
    /// ```
    fn expect_err<+PanicDestruct<T>>(self: Result<T, E>, err: felt252) -> E {
        match self {
            Result::Ok(_) => crate::panic_with_felt252(err),
            Result::Err(x) => x,
        }
    }

    /// Returns the contained `Err` value, consuming the `self` value.
    ///
    /// # Panics
    ///
    /// Panics if the value is an `Ok`, with a standard `Result::unwrap_err failed.` panic message.
    ///
    /// # Examples
    ///
    /// ```
    /// let result: Result<felt252, felt252> = Result::Err('no value');
    /// assert!(result.unwrap_err() == 'no value');
    /// ```
    fn unwrap_err<+PanicDestruct<T>>(self: Result<T, E>) -> E {
        self.expect_err('Result::unwrap_err failed.')
    }

    /// Returns `true` if the `Result` is `Result::Ok`.
    ///
    /// # Examples
    ///
    /// ```
    /// let result: Result<felt252, felt252> = Result::Ok(123);
    /// assert!(result.is_ok());
    /// ```
    #[inline]
    fn is_ok(self: @Result<T, E>) -> bool {
        match self {
            Result::Ok(_) => true,
            Result::Err(_) => false,
        }
    }

    /// Returns `true` if the `Result` is `Result::Err`.
    ///
    /// # Examples
    ///
    /// ```
    /// let result: Result<felt252, felt252> = Result::Ok(123);
    /// assert!(!result.is_err());
    /// ```
    #[inline]
    fn is_err(self: @Result<T, E>) -> bool {
        match self {
            Result::Ok(_) => false,
            Result::Err(_) => true,
        }
    }

    /// Returns `true` if the `Result` is `Result::Ok`, and consumes the value.
    ///
    /// # Examples
    ///
    /// ```
    /// let result: Result<felt252, felt252> = Result::Ok(123);
    /// assert!(result.into_is_ok());
    /// ```
    #[inline]
    fn into_is_ok<+Destruct<T>, +Destruct<E>>(self: Result<T, E>) -> bool {
        match self {
            Result::Ok(_) => true,
            Result::Err(_) => false,
        }
    }

    /// Returns `true` if the `Result` is `Result::Err`, and consumes the value.
    ///
    /// # Examples
    ///
    /// ```
    /// let result: Result<felt252, felt252> = Result::Ok(123);
    /// assert!(!result.into_is_err());
    /// ```
    #[inline]
    fn into_is_err<+Destruct<T>, +Destruct<E>>(self: Result<T, E>) -> bool {
        match self {
            Result::Ok(_) => false,
            Result::Err(_) => true,
        }
    }

    /// Converts from `Result<T, E>` to `Option<T>`.
    ///
    /// Converts `self` into an `Option<T>`, consuming `self`,
    /// and discarding the error, if any.
    ///
    /// # Examples
    ///
    /// ```
    /// let x: Result<u32, ByteArray> = Result::Ok(2);
    /// assert!(x.ok() == Option::Some(2));
    ///
    /// let x: Result<u32, ByteArray> = Result::Err("Nothing here");
    /// assert!(x.ok().is_none());
    /// ```
    fn ok<+Destruct<T>, +Destruct<E>>(self: Result<T, E>) -> Option<T> {
        match self {
            Result::Ok(x) => Option::Some(x),
            Result::Err(_) => Option::None,
        }
    }

    /// Converts from `Result<T, E>` to `Option<E>`.
    ///
    /// Converts `self` into an `Option<E>`, consuming `self`,
    /// and discarding the success value, if any.
    ///
    /// # Examples
    ///
    /// ```
    /// let x: Result<u32, ByteArray> = Result::Err("Nothing here");
    /// assert!(x.err() == Option::Some("Nothing here"));
    ///
    /// let x: Result<u32, ByteArray> = Result::Ok(2);
    /// assert!(x.err().is_none());
    /// ```
    fn err<+Destruct<T>, +Destruct<E>>(self: Result<T, E>) -> Option<E> {
        match self {
            Result::Ok(_) => Option::None,
            Result::Err(x) => Option::Some(x),
        }
    }
}
