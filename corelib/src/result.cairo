//! Error handling with the `Result` type.
//!
//! [`Result`] is the type used for returning and propagating
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
//! These methods extract the contained value in a [`Result<T, E>`] when it
//! is the [`Ok`] variant. If the [`Result`] is [`Err`]:
//!
//! * [`expect`] panics with a provided felt252 error message
//! * [`unwrap`] panics with a generic message
//! * [`unwrap_or`] returns the provided default value
//! * [`unwrap_or_default`] returns the default value of the type `T` (which must implement the
//! [`Default`] trait)
//! * [`unwrap_or_else`] returns the result of evaluating the provided function
//!
//! [`expect`]: ResultTrait::expect
//! [`unwrap`]: ResultTrait::unwrap
//! [`unwrap_or`]: ResultTrait::unwrap_or
//! [`unwrap_or_default`]: ResultTrait::unwrap_or_default
//! [`unwrap_or_else`]: ResultTrait::unwrap_or_else
//!
//! These methods extract the contained value in a [`Result<T, E>`] when it
//! is the [`Err`] variant. If the [`Result`] is [`Ok`]:
//!
//! * [`expect_err`] panics with a provided felt252 error message
//! * [`unwrap_err`] panics with a generic message
//!
//! [`expect_err`]: ResultTrait::expect_err
//! [`unwrap_err`]: ResultTrait::unwrap_err
//!
//! ## Transforming contained values
//!
//! * [`ok`] transforms [`Result<T, E>`] into [`Option<T>`], mapping
//!   [`Ok(v)`] to [`Some(v)`] and [`Err(e)`] to [`None`]
//! * [`err`] transforms [`Result<T, E>`] into [`Option<E>`], mapping
//!   [`Ok(v)`] to [`None`] and [`Err(e)`] to [`Some(e)`]
//!
//! ## Boolean operators
//!
//! These methods treat the [`Result`] as a boolean value, where [`Ok`]
//! acts like [`true`] and [`Err`] acts like [`false`]. There are two
//! categories of these methods: ones that take a [`Result`] as input, and
//! ones that take a function as input.
//!
//! The [`and`] and [`or`] methods take another [`Result`] as input, and
//! produce a [`Result`] as output. The [`and`] method can produce a
//! [`Result<U, E>`] value having a different inner type `U` than
//! [`Result<T, E>`]. The [`or`] method can produce a [`Result<T, F>`]
//! value having a different error type `F` than [`Result<T, E>`].
//!
//! | method  | self     | input     | output   |
//! |---------|----------|-----------|----------|
//! | [`and`] | `Err(e)` | (ignored) | `Err(e)` |
//! | [`and`] | `Ok(x)`  | `Err(d)`  | `Err(d)` |
//! | [`and`] | `Ok(x)`  | `Ok(y)`   | `Ok(y)`  |
//! | [`or`]  | `Err(e)` | `Err(d)`  | `Err(d)` |
//! | [`or`]  | `Err(e)` | `Ok(y)`   | `Ok(y)`  |
//! | [`or`]  | `Ok(x)`  | (ignored) | `Ok(x)`  |
//!
//! [`and`]: ResultTrait::and
//! [`or`]: ResultTrait::or
//!
//! The [`and_then`] and [`or_else`] methods take a function as input, and
//! only evaluate the function when they need to produce a new value. The
//! [`and_then`] method can produce a [`Result<U, E>`] value having a
//! different inner type `U` than [`Result<T, E>`]. The [`or_else`] method
//! can produce a [`Result<T, F>`] value having a different error type `F`
//! than [`Result<T, E>`].
//!
//! | method       | self     | function input | function result | output   |
//! |--------------|----------|----------------|-----------------|----------|
//! | [`and_then`] | `Err(e)` | (not provided) | (not evaluated) | `Err(e)` |
//! | [`and_then`] | `Ok(x)`  | `x`            | `Err(d)`        | `Err(d)` |
//! | [`and_then`] | `Ok(x)`  | `x`            | `Ok(y)`         | `Ok(y)`  |
//! | [`or_else`]  | `Err(e)` | `e`            | `Err(d)`        | `Err(d)` |
//! | [`or_else`]  | `Err(e)` | `e`            | `Ok(y)`         | `Ok(y)`  |
//! | [`or_else`]  | `Ok(x)`  | (not provided) | (not evaluated) | `Ok(x)`  |
//!
//! [`and_then`]: ResultTrait::and_then
//! [`or_else`]: ResultTrait::or_else
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
//!
//! ## Iterating over `Result`
//!
//! A [`Result`] can be iterated over. This can be helpful if you need an
//! iterator that is conditionally empty. The iterator will either produce
//! a single value (when the [`Result`] is [`Ok`]), or produce no values
//! (when the [`Result`] is [`Err`]). For example, [`into_iter`]
//! contains [`Some(v)`] if the [`Result`] is [`Ok(v)`], and [`None`] if the
//! [`Result`] is [`Err`].
//!
//! [`into_iter`]: IntoIterator::into_iter

#[allow(unused_imports)]
use crate::array::{ArrayTrait, SpanTrait};
#[allow(unused_imports)]
use crate::serde::Serde;

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

    /// Returns the contained [`Ok`] value or computes it from a closure.
    ///
    ///
    /// # Examples
    ///
    /// ```
    /// assert!(Result::Ok(2).unwrap_or_else(|e: ByteArray| e.len()) == 2);
    /// assert!(Result::Err("foo").unwrap_or_else(|e: ByteArray| e.len()) == 3);
    /// ```
    #[inline]
    fn unwrap_or_else<F, +Destruct<E>, +Drop<F>, +core::ops::FnOnce<F, (E,)>[Output: T]>(
        self: Result<T, E>, f: F,
    ) -> T {
        match self {
            Result::Ok(x) => x,
            Result::Err(e) => f(e),
        }
    }

    /// Returns `other` if the result is `Ok`, otherwise returns the `Err` value of `self`.
    ///
    /// # Examples
    ///
    /// ```
    /// let x: Result<u32, ByteArray> = Result::Ok(2);
    /// let y: Result<ByteArray, ByteArray> = Result::Err("late error");
    /// assert!(x.and(y) == Result::Err("late error"));
    ///
    /// let x: Result<u32, ByteArray> = Result::Err("early error");
    /// let y: Result<ByteArray, ByteArray> = Result::Ok("foo");
    /// assert!(x.and(y) == Result::Err("early error"));
    ///
    /// let x: Result<u32, ByteArray> = Result::Err("not a 2");
    /// let y: Result<ByteArray, ByteArray> = Result::Err("late error");
    /// assert!(x.and(y) == Result::Err("not a 2"));
    ///
    /// let x: Result<u32, ByteArray> = Result::Ok(2);
    /// let y: Result<ByteArray, ByteArray> = Result::Ok("different result type");
    /// assert!(x.and(y) == Result::Ok("different result type"));
    /// ```
    #[inline]
    fn and<U, +Destruct<T>, +Drop<E>, +Drop<U>>(
        self: Result<T, E>, other: Result<U, E>,
    ) -> Result<U, E> {
        match self {
            Result::Ok(_) => other,
            Result::Err(e) => Result::Err(e),
        }
    }

    /// Calls `op` if the result is `Ok`, otherwise returns the `Err` value of `self`.
    ///
    /// This function can be used for control flow based on `Result` values.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::CheckedMul;
    ///
    /// fn sq_then_string(x: u32) -> Result<ByteArray, ByteArray> {
    ///     let res = x.checked_mul(x).ok_or("overflowed");
    ///     res.and_then(|v| Result::Ok(format!("{}", v)))
    /// }
    ///
    /// let x = sq_then_string(4);
    /// assert!(x == Result::Ok("16"));
    ///
    /// let y = sq_then_string(65536);
    /// assert!(y == Result::Err("overflowed"));
    /// ```
    #[inline]
    fn and_then<U, F, +Drop<F>, +core::ops::FnOnce<F, (T,)>[Output: Result<U, E>]>(
        self: Result<T, E>, op: F,
    ) -> Result<U, E> {
        match self {
            Result::Ok(t) => op(t),
            Result::Err(e) => Result::Err(e),
        }
    }

    /// Returns `other` if the result is `Err`, otherwise returns the `Ok` value of `self`.
    ///
    /// # Examples
    ///
    /// ```
    /// let x: Result<u32, ByteArray> = Result::Ok(2);
    /// let y: Result<u32, ByteArray> = Result::Err("late error");
    /// assert!(x.or(y) == Result::Ok(2));
    ///
    /// let x: Result<u32, ByteArray> = Result::Err("early error");
    /// let y: Result<u32, ByteArray> = Result::Ok(2);
    /// assert!(x.or(y) == Result::Ok(2));
    ///
    /// let x: Result<u32, ByteArray> = Result::Err("not a 2");
    /// let y: Result<u32, ByteArray> = Result::Err("late error");
    /// assert!(x.or(y) == Result::Err("late error"));
    ///
    /// let x: Result<u32, ByteArray> = Result::Ok(2);
    /// let y: Result<u32, ByteArray> = Result::Ok(100);
    /// assert!(x.or(y) == Result::Ok(2));
    /// ```
    #[inline]
    fn or<F, +Drop<T>, +Drop<F>, +Destruct<E>>(
        self: Result<T, E>, other: Result<T, F>,
    ) -> Result<T, F> {
        match self {
            Result::Ok(v) => Result::Ok(v),
            Result::Err(_) => other,
        }
    }

    /// Calls `op` if the result is `Err`, otherwise returns the `Ok` value of `self`.
    ///
    /// This function can be used for control flow based on result values.
    ///
    /// # Examples
    ///
    /// ```
    /// let x: Result::<u32, ByteArray> = Result::<u32, ByteArray>::Err("bad input")
    ///     .or_else(|_e| Result::Ok(42));
    /// assert!(x == Result::Ok(42));
    ///
    /// let y: Result::<u32, ByteArray> = Result::<u32, ByteArray>::Err("bad input")
    ///     .or_else(|_e| Result::Err("not 42"));
    /// assert!(y == Result::Err("not 42"));
    ///
    /// let z: Result::<u32, ByteArray> = Result::<u32, ByteArray>::Ok(100)
    ///     .or_else(|_e| Result::Ok(42));
    /// assert!(z == Result::Ok(100));
    /// ```
    #[inline]
    fn or_else<F, O, +Drop<O>, +core::ops::FnOnce<O, (E,)>[Output: Result<T, F>]>(
        self: Result<T, E>, op: O,
    ) -> Result<T, F> {
        match self {
            Result::Ok(t) => Result::Ok(t),
            Result::Err(e) => op(e),
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


impl ResultIntoIterator<
    T, E, +Destruct<T>, +Destruct<E>,
> of crate::iter::IntoIterator<Result<T, E>> {
    type IntoIter = crate::option::OptionIter<T>;

    /// Returns a consuming iterator over the possibly contained value.
    ///
    /// The iterator yields one value if the result is [`Result::Ok`], otherwise none.
    ///
    /// # Examples
    ///
    /// ```
    /// let x: Result<u32, ByteArray> = Result::Ok(5);
    /// let mut x_iter = x.into_iter();
    /// assert!(x_iter.next() == Option::Some(5));
    ///
    /// let x: Result<u32, ByteArray> = Result::Err("nothing!");
    /// let mut x_iter = x.into_iter();
    /// assert!(x_iter.next() == Option::None);
    /// ```
    #[inline]
    fn into_iter(self: Result<T, E>) -> crate::option::OptionIter<T> {
        self.ok().into_iter()
    }
}
