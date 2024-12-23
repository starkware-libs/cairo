//! Optional values.
//!
//! The [`Option`] type represents an optional value: every [`Option`] is either [`Some`] and
//! contains a value, or [`None`], and does not. [`Option`] types are very common in Cairo code, as
//! they have a number of uses:
//!
//! * Initial values
//! * Return values for functions that are not defined
//!   over their entire input range (partial functions)
//! * Return value for otherwise reporting simple errors, where [`None`] is
//!   returned on error
//! * Optional struct fields
//! * Optional function arguments
//!
//! Options are commonly paired with pattern matching to query the presence of a value and take
//! action, always accounting for the `None` case.
//!
//! ```
//! fn divide(numerator: u64, denominator: u64) -> Option<u64> {
//!     if denominator == 0 {
//!         Option::None
//!     } else {
//!         Option::Some(numerator / denominator)
//!     }
//! }
//!
//! // The return value of the function is an option
//! let result = divide(2, 3);
//!
//! // Pattern match to retrieve the value
//! match result {
//!     // The division was valid
//!     Option::Some(x) => println!("Result: {x}"),
//!     // The division was invalid
//!     Option::None    => println!("Cannot divide by 0"),
//! }
//! ```
//!
//! # The question mark operator, `?`
//!
//! Similar to the [`Result`] type, when writing code that calls many functions that return the
//! [`Option`] type, handling `Some`/`None` can be tedious. The question mark
//! operator, `?`, hides some of the boilerplate of propagating values
//! up the call stack.
//!
//! It replaces this:
//!
//! ```
//! fn add_last_numbers(mut array: Array<u32>) -> Option<u32> {
//!     let a = array.pop_front();
//!     let b = array.pop_front();
//!
//!     match (a, b) {
//!         (Option::Some(x), Option::Some(y)) => Option::Some(x + y),
//!         _ => Option::None,
//!     }
//! }
//!
//! ```
//!
//! With this:
//!
//! ```
//! fn add_last_numbers(mut array: Array<u32>) -> Option<u32> {
//!     Option::Some(array.pop_front()? + array.pop_front()?)
//!  }
//! ```
//!
//! *It's much nicer!*
//!
//! Ending the expression with `?` will result in the [`Some`]'s unwrapped value, unless the
//! result is [`None`], in which case [`None`] is returned early from the enclosing function.
//! `?` can be used in functions that return [`Option`] because of the
//! early return of [`None`] that it provides.
//!
//! [`Some`]: Option::Some
//! [`None`]: Option::None
//!
//! # Method overview
//!
//! In addition to working with pattern matching, [`Option`] provides a wide
//! variety of different methods.
//!
//! ## Querying the variant
//!
//! The [`is_some`] and [`is_none`] methods return `true` if the [`Option`]
//! is [`Some`] or [`None`], respectively.
//!
//! [`is_none`]: OptionTrait::is_none
//! [`is_some`]: OptionTrait::is_some
//!
//! ## Extracting the contained value
//!
//! These methods extract the contained value in an [`Option<T>`] when it
//! is the [`Some`] variant. If the [`Option`] is [`None`]:
//!
//! * [`expect`] panics with a provided custom message
//! * [`unwrap`] panics with a generic message
//! * [`unwrap_or`] returns the provided default value
//! * [`unwrap_or_default`] returns the default value of the type `T`
//!   (which must implement the [`Default`] trait)
//! * [`unwrap_or_else`] returns the result of evaluating the provided
//!   function
//!
//! [`expect`]: OptionTrait::expect
//! [`unwrap`]: OptionTrait::unwrap
//! [`unwrap_or`]: OptionTrait::unwrap_or
//! [`unwrap_or_default`]: OptionTrait::unwrap_or_default
//! [`unwrap_or_else`]: OptionTrait::unwrap_or_else
//!
//! ## Transforming contained values
//!
//! These methods transform [`Option`] to [`Result`]:
//!
//! * [`ok_or`] transforms [`Some(v)`] to [`Ok(v)`], and [`None`] to
//!   [`Err(err)`] using the provided default `err` value.
//!
//! [`Err(err)`]: Result::Err
//! [`Ok(v)`]: Result::Ok
//! [`Some(v)`]: Option::Some
//! [`ok_or`]: OptionTrait::ok_or

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

    /// Returns the contained [`Some`] value or computes it from a closure.
    ///
    /// # Examples
    ///
    /// ```
    /// let k = 10;
    /// assert!(Option::Some(4).unwrap_or_else(|| 2 * k) == 4);
    /// assert!(Option::None.unwrap_or_else(|| 2 * k) == 20);
    /// ```
    fn unwrap_or_else<
        F, +Drop<F>, impl func: core::ops::FnOnce<F, ()>[Output: T], +Drop<func::Output>,
    >(
        self: Option<T>, f: F,
    ) -> T;
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

    #[inline]
    fn unwrap_or_else<
        F, +Drop<F>, impl func: core::ops::FnOnce<F, ()>[Output: T], +Drop<func::Output>,
    >(
        self: Option<T>, f: F,
    ) -> T {
        match self {
            Option::Some(x) => x,
            Option::None => f(),
        }
    }
}
