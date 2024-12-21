//! Error handling with the `Result` type.
//!
//! [`Result<T, E>`][`Result`] is the type used for returning and propagating
//! errors. It is an enum with the variants, [`Ok(T)`], representing
//! success and containing a value, and [`Err(E)`], representing error
//! and containing an error value.
//!
//! ```
//! enum Result<T, E> {
//!    Ok: T,
//!    Err: E,
//! }
//! ```
//!
//! Functions return [`Result`] whenever errors are expected and
//! recoverable.
//!
//! A simple function returning [`Result`] might be
//! defined and used like so:
//!
//! ```
//! fn parse_version(header: felt252) -> Result<felt252, felt252> {
//!     match header {
//!         0 => Result::Ok(0),
//!         1 => Result::Ok(1),
//!         _ => Result::Err('invalid version'),
//!     }
//! }
//!
//! let version = parse_version(1);
//! match version {
//!     Result::Ok(v) => println!("working with version {}", v),
//!     Result::Err(e) => println!("error parsing version: {:?}", e)
//! }
//! ```
//!
//! # Results must be used
//!
//! A common problem with using return values to indicate errors is
//! that it is easy to ignore the return value, thus failing to handle
//! the error. [`Result`] is annotated with the `#[must_use]` attribute,
//! which will cause the compiler to issue a warning when a Result
//! value is ignored.
//!
//! # Method overview
//!
//! In addition to working with pattern matching, [`Result`] provides a wide
//! variety of different methods.
//!
//! ## Querying the variant
//!
//! The [`is_ok`] and [`is_err`] methods return `true` if the [`Result`]
//! is [`Ok`] or [`Err`], respectively.
//!
//! ## Extracting contained values
//!
//! These methods extract the contained value in a [`Result<T, E>`]:
//!
//! * [`expect`] returns the contained value if [`Ok`], otherwise panics with a provided
//! custom message
//! * [`unwrap`] returns the contained value if [`Ok`], otherwise panics with a
//! generic message
//! * [`unwrap_or`] returns the contained value if [`Ok`], otherwise returns the provided default
//! value
//! * [`unwrap_or_default`] returns the contained value if [`Ok`], otherwise returns the default
//! value for the type
//!
//! ## Converting between Result and Option
//!
//! * [`ok`] transforms [`Result<T, E>`] into [`Option<T>`], mapping
//!   [`Ok(v)`] to [`Some(v)`] and [`Err(_)`] to [`None`]
//! * [`err`] transforms [`Result<T, E>`] into [`Option<E>`], mapping
//!   [`Ok(_)`] to [`None`] and [`Err(e)`] to [`Some(e)`]
//!
//! # The question mark operator, `?`
//!
//! When writing code that calls many functions that return the [`Result`] type,
//! handling `Ok`/`Err` can be tedious. The question mark operator, `?`,
//! hides some of the boilerplate of propagating errors up the call stack.
//!
//! It replaces this:
//!
//! ```
//! use core::integer::u8_overflowing_add;
//!
//! fn add_three_numbers(a: u8, b: u8, c: u8) -> Result<u8, u8> {
//!     match u8_overflowing_add(a, b) {
//!         Result::Ok(sum_ab) => {
//!             match u8_overflowing_add(sum_ab, c) {
//!                 Result::Ok(total) => Result::Ok(total),
//!                 Result::Err(e) => Result::Err(e),
//!             }
//!         },
//!         Result::Err(e) => Result::Err(e),
//!     }
//! }
//! ```
//!
//! With this:
//!
//! ```
//! use core::integer::u8_overflowing_add;
//!
//! fn add_three_numbers_2(a: u8, b: u8, c: u8) -> Result<u8, u8> {
//!     let total = u8_overflowing_add(u8_overflowing_add(a, b)?, c)?;
//!     Result::Ok(total)
//! }
//! ```
//!
//! *It's much nicer!*
//!
//! [`Ok`]: Result::Ok
//! [`Err`]: Result::Err

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
