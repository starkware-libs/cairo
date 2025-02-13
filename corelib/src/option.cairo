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
//!         None
//!     } else {
//!         Some(numerator / denominator)
//!     }
//! }
//!
//! // The return value of the function is an option
//! let result = divide(2, 3);
//!
//! // Pattern match to retrieve the value
//! match result {
//!     // The division was valid
//!     Some(x) => println!("Result: {x}"),
//!     // The division was invalid
//!     None    => println!("Cannot divide by 0"),
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
//!         (Some(x), Some(y)) => Some(x + y),
//!         _ => None,
//!     }
//! }
//!
//! ```
//!
//! With this:
//!
//! ```
//! fn add_last_numbers(mut array: Array<u32>) -> Option<u32> {
//!     Some(array.pop_front()? + array.pop_front()?)
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
//! [`Some`]: Some
//! [`None`]: None
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
//! [`is_none_or`]: OptionTrait::is_none_or
//! [`is_some`]: OptionTrait::is_some
//! [`is_some_and`]: OptionTrait::is_some_and
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
//! * [`ok_or_else`] transforms [`Some(v)`] to [`Ok(v)`], and [`None`] to
//!   a value of [`Err`] using the provided function
//!
//! [`Err(err)`]: Err
//! [`Ok(v)`]: Ok
//! [`Some(v)`]: Some
//! [`ok_or`]: OptionTrait::ok_or
//! [`ok_or_else`]: OptionTrait::ok_or_else
//!
//! These methods transform the [`Some`] variant:
//!
//! * [`map`] transforms [`Option<T>`] to [`Option<U>`] by applying the
//!   provided function to the contained value of [`Some`] and leaving
//!   [`None`] values unchanged
//!
//! [`map`]: OptionTrait::map
//!
//! These methods transform [`Option<T>`] to a value of a possibly
//! different type `U`:
//!
//! * [`map_or`] applies the provided function to the contained value of
//!   [`Some`], or returns the provided default value if the [`Option`] is
//!   [`None`]
//! * [`map_or_else`] applies the provided function to the contained value
//!   of [`Some`], or returns the result of evaluating the provided
//!   fallback function if the [`Option`] is [`None`]
//!
//! [`map_or`]: OptionTrait::map_or
//! [`map_or_else`]: OptionTrait::map_or_else
//!
//! ## Boolean operators
//!
//! These methods treat the [`Option`] as a boolean value, where [`Some`]
//! acts like [`true`] and [`None`] acts like [`false`]. There are two
//! categories of these methods: ones that take an [`Option`] as input, and
//! ones that take a function as input (to be lazily evaluated).
//!
//! The [`and`], [`or`], and [`xor`] methods take another [`Option`] as
//! input, and produce an [`Option`] as output. Only the [`and`] method can
//! produce an [`Option<U>`] value having a different inner type `U` than
//! [`Option<T>`].
//!
//! | method  | self      | input     | output    |
//! |---------|-----------|-----------|-----------|
//! | [`and`] | `None`    | (ignored) | `None`    |
//! | [`and`] | `Some(x)` | `None`    | `None`    |
//! | [`and`] | `Some(x)` | `Some(y)` | `Some(y)` |
//! | [`or`]  | `None`    | `None`    | `None`    |
//! | [`or`]  | `None`    | `Some(y)` | `Some(y)` |
//! | [`or`]  | `Some(x)` | (ignored) | `Some(x)` |
//! | [`xor`] | `None`    | `None`    | `None`    |
//! | [`xor`] | `None`    | `Some(y)` | `Some(y)` |
//! | [`xor`] | `Some(x)` | `None`    | `Some(x)` |
//! | [`xor`] | `Some(x)` | `Some(y)` | `None`    |
//!
//! [`and`]: OptionTrait::and
//! [`or`]: OptionTrait::or
//! [`xor`]: OptionTrait::xor
//!
//! The [`and_then`] and [`or_else`] methods take a function as input, and
//! only evaluate the function when they need to produce a new value. Only
//! the [`and_then`] method can produce an [`Option<U>`] value having a
//! different inner type `U` than [`Option<T>`].
//!
//! | method       | self      | function input | function result | output    |
//! |--------------|-----------|----------------|-----------------|-----------|
//! | [`and_then`] | `None`    | (not provided) | (not evaluated) | `None`    |
//! | [`and_then`] | `Some(x)` | `x`            | `None`          | `None`    |
//! | [`and_then`] | `Some(x)` | `x`            | `Some(y)`       | `Some(y)` |
//! | [`or_else`]  | `None`    | (not provided) | `None`          | `None`    |
//! | [`or_else`]  | `None`    | (not provided) | `Some(y)`       | `Some(y)` |
//! | [`or_else`]  | `Some(x)` | (not provided) | (not evaluated) | `Some(x)` |
//!
//! [`and_then`]: OptionTrait::and_then
//! [`or_else`]: OptionTrait::or_else
//! ## Iterating over `Option`
//!
//! An [`Option`] can be iterated over. This can be helpful if you need an
//! iterator that is conditionally empty. The iterator will either produce
//! a single value (when the [`Option`] is [`Some`]), or produce no values
//! (when the [`Option`] is [`None`]). For example, [`into_iter`]
//! contains [`Some(v)`] if the [`Option`] is [`Some(v)`], and [`None`] if the
//! [`Option`] is [`None`].
//!
//! [`into_iter`]: IntoIterator::into_iter

/// The `Option<T>` enum representing either `Some(value)` or `None`.
#[must_use]
#[derive(Copy, Drop, Debug, Serde, PartialEq)]
pub enum Option<T> {
    Some: T,
    None,
}

impl OptionDefault<T> of Default<Option<T>> {
    fn default() -> Option<T> {
        None
    }
}

pub impl DestructOption<T, +Destruct<T>, -Drop<Option<T>>> of Destruct<Option<T>> {
    #[inline]
    fn destruct(self: Option<T>) nopanic {
        match self {
            Some(x) => x.destruct(),
            None => (),
        }
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
    /// let option = Some(123);
    /// let value = option.expect('no value');
    /// assert!(value == 123);
    /// ```
    const fn expect(self: Option<T>, err: felt252) -> T;

    /// Returns the contained `Some` value, consuming the `self` value.
    ///
    /// # Panics
    ///
    /// Panics if the `self` value equals `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// let option = Some(123);
    /// let value = option.unwrap();
    /// assert!(value == 123);
    /// ```
    const fn unwrap(self: Option<T>) -> T;

    /// Transforms the `Option<T>` into a `Result<T, E>`, mapping `Some(v)` to
    /// `Ok(v)` and `None` to `Err(err)`.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Some('foo').ok_or(0), Ok('foo'));
    ///
    /// let option: Option<felt252> = None;
    /// assert_eq!(option.ok_or(0), Err(0));
    /// ```
    fn ok_or<E, +Destruct<E>>(self: Option<T>, err: E) -> Result<T, E>;

    /// Transforms the `Option<T>` into a `Result<T, E>`, mapping `Some(v)` to
    /// `Ok(v)` and `None` to `Err(err())`.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Some('foo').ok_or_else(|| 0), Ok('foo'));
    ///
    /// let option: Option<felt252> = None;
    /// assert_eq!(option.ok_or_else(|| 0), Err(0));
    /// ```
    fn ok_or_else<E, F, +Destruct<E>, +core::ops::FnOnce<F, ()>[Output: E], +Drop<F>>(
        self: Option<T>, err: F,
    ) -> Result<T, E>;

    /// Returns [`None`] if the option is [`None`], otherwise returns `optb`.
    ///
    /// Arguments passed to `and` are eagerly evaluated; if you are passing the
    /// result of a function call, it is recommended to use [`and_then`], which is
    /// lazily evaluated.
    ///
    /// [`and_then`]: OptionTrait::and_then
    ///
    /// # Examples
    ///
    /// ```
    /// let x = Some(2);
    /// let y: Option<ByteArray> = None;
    /// assert_eq!(x.and(y), None);
    ///
    /// let x: Option<u32> = None;
    /// let y: Option<ByteArray> = Some("foo");
    /// assert_eq!(x.and(y), None);
    ///
    /// let x = Some(2);
    /// let y: Option<ByteArray> = Some("foo");
    /// assert_eq!(x.and(y), Some("foo"));
    ///
    /// let x: Option<u32> = None;
    /// let y: Option<ByteArray> = None;
    /// assert_eq!(x.and(y), None);
    /// ```
    fn and<U, +Drop<T>, +Drop<U>>(self: Option<T>, optb: Option<U>) -> Option<U>;

    /// Returns [`None`] if the option is [`None`], otherwise calls `f` with the
    /// wrapped value and returns the result.
    ///
    /// Some languages call this operation flatmap.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::num::traits::CheckedMul;
    ///
    /// let option: Option<ByteArray> = checked_mul(2_u32, 2_u32)
    ///     .and_then(|v| Some(format!("{}", v)));
    /// assert_eq!(option, Some("4"));
    ///
    /// let option: Option<ByteArray> = checked_mul(65536_u32, 65536_u32)
    ///     .and_then(|v| Some(format!("{}", v)));
    /// assert_eq!(option, None); // overflowed!
    ///
    /// let option: Option<ByteArray> = Option::<u32>::None
    ///     .and_then(|v| Some(format!("{}", v)));
    /// assert_eq!(option, None);
    /// ```
    fn and_then<U, F, +Drop<F>, +core::ops::FnOnce<F, (T,)>[Output: Option<U>]>(
        self: Option<T>, f: F,
    ) -> Option<U>;

    /// Returns the option if it contains a value, otherwise returns `optb`.
    ///
    /// Arguments passed to `or` are eagerly evaluated; if you are passing the
    /// result of a function call, it is recommended to use [`or_else`], which is
    /// lazily evaluated.
    ///
    /// [`or_else`]: OptionTrait::or_else
    ///
    /// # Examples
    ///
    /// ```
    /// let x = Some(2);
    /// let y = None;
    /// assert_eq!(x.or(y), Some(2));
    ///
    /// let x = None;
    /// let y = Some(100);
    /// assert_eq!(x.or(y), Some(100));
    ///
    /// let x = Some(2);
    /// let y = Some(100);
    /// assert_eq!(x.or(y), Some(2));
    ///
    /// let x: Option<u32> = None;
    /// let y = None;
    /// assert_eq!(x.or(y), None);
    /// ```
    fn or<+Drop<T>>(self: Option<T>, optb: Option<T>) -> Option<T>;

    /// Returns the option if it contains a value, otherwise calls `f` and
    /// returns the result.
    ///
    /// # Examples
    ///
    /// ```
    /// let nobody = || Option::<ByteArray>::None;
    /// let vikings = || Option::<ByteArray>::Some("vikings");
    ///
    /// assert_eq!(Some("barbarians").or_else(vikings), Some("barbarians"));
    /// assert_eq!(None.or_else(vikings), Some("vikings"));
    /// assert_eq!(None.or_else(nobody), None);
    /// ```
    fn or_else<F, +Drop<F>, +core::ops::FnOnce<F, ()>[Output: Option<T>]>(
        self: Option<T>, f: F,
    ) -> Option<T>;

    /// Returns [`Some`] if exactly one of `self`, `optb` is [`Some`], otherwise returns [`None`].
    ///
    /// # Examples
    ///
    /// ```
    /// let x = Some(2);
    /// let y: Option<u32> = None;
    /// assert_eq!(x.xor(y), Some(2));
    ///
    /// let x: Option<u32> = None;
    /// let y = Some(2);
    /// assert_eq!(x.xor(y), Some(2));
    ///
    /// let x = Some(2);
    /// let y = Some(2);
    /// assert_eq!(x.xor(y), None);
    ///
    /// let x: Option<u32> = None;
    /// let y: Option<u32> = None;
    /// assert_eq!(x.xor(y), None);
    /// ```
    fn xor<+Drop<T>>(self: Option<T>, optb: Option<T>) -> Option<T>;

    /// Returns `true` if the `Option` is `Some`, `false` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// let option = Some(123);
    /// assert!(option.is_some());
    /// ```
    #[must_use]
    fn is_some(self: @Option<T>) -> bool;

    /// Returns `true` if the `Option` is `Some` and the value inside of it matches a
    /// predicate.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Some(2_u8).is_some_and(|x| x > 1), true);
    /// assert_eq!(Some(0_u8).is_some_and(|x| x > 1), false);
    ///
    /// let option: Option<u8> = None;
    /// assert_eq!(option.is_some_and(|x| x > 1), false);
    /// ```
    #[must_use]
    fn is_some_and<F, +Drop<F>, +core::ops::FnOnce<F, (T,)>[Output: bool]>(
        self: Option<T>, f: F,
    ) -> bool;

    /// Returns `true` if the `Option` is `None`, `false` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// let option = Some(123);
    /// assert!(!option.is_none());
    /// ```
    #[must_use]
    fn is_none(self: @Option<T>) -> bool;

    /// Returns `true` if the `Option` is `None` or the value inside of it matches a
    /// predicate.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Some(2_u8).is_none_or(|x| x > 1), true);
    /// assert_eq!(Some(0_u8).is_none_or(|x| x > 1), false);
    ///
    /// let option: Option<u8> = None;
    /// assert_eq!(option.is_none_or(|x| x > 1), true);
    /// ```
    #[must_use]
    fn is_none_or<F, +Drop<F>, +core::ops::FnOnce<F, (T,)>[Output: bool]>(
        self: Option<T>, f: F,
    ) -> bool;

    /// Returns the contained `Some` value if `self` is `Some(x)`. Otherwise, returns the
    /// provided default.
    ///
    /// # Examples
    ///
    /// ```
    /// let option = Some(123);
    /// assert!(option.unwrap_or(456) == 123);
    ///
    /// let option = None;
    /// assert!(option.unwrap_or(456) == 456);
    /// ```
    const fn unwrap_or<+Destruct<T>>(self: Option<T>, default: T) -> T;

    /// Returns the contained `Some` value if `self` is `Some(x)`. Otherwise, returns
    /// `Default::<T>::default()`.
    ///
    /// # Examples
    ///
    /// ```
    /// let option = Some(123);
    /// assert!(option.unwrap_or_default() == 123);
    ///
    /// let option: Option<felt252> = None;
    /// assert!(option.unwrap_or_default() == Default::default());
    /// ```
    fn unwrap_or_default<+Default<T>>(self: Option<T>) -> T;

    /// Returns the contained [`Some`] value or computes it from a closure.
    ///
    /// # Examples
    ///
    /// ```
    /// let k = 10;
    /// assert!(Some(4).unwrap_or_else(|| 2 * k) == 4);
    /// assert!(None.unwrap_or_else(|| 2 * k) == 20);
    /// ```
    fn unwrap_or_else<
        F, +Drop<F>, impl func: core::ops::FnOnce<F, ()>[Output: T], +Drop<func::Output>,
    >(
        self: Option<T>, f: F,
    ) -> T;

    /////////////////////////////////////////////////////////////////////////
    // Transforming contained values
    /////////////////////////////////////////////////////////////////////////

    /// Maps an `Option<T>` to `Option<U>` by applying a function to a contained value (if `Some`)
    /// or returns `None` (if `None`).
    ///
    /// # Examples
    ///
    /// ```
    /// let maybe_some_string: Option<ByteArray> = Some("Hello, World!");
    /// // `Option::map` takes self *by value*, consuming `maybe_some_string`
    /// let maybe_some_len = maybe_some_string.map(|s: ByteArray| s.len());
    /// assert!(maybe_some_len == Some(13));
    ///
    /// let x: Option<ByteArray> = None;
    /// assert!(x.map(|s: ByteArray| s.len()) == None);
    /// ```
    fn map<U, F, +Destruct<F>, +core::ops::FnOnce<F, (T,)>[Output: U]>(
        self: Option<T>, f: F,
    ) -> Option<U>;

    /// Returns the provided default result (if none),
    /// or applies a function to the contained value (if any).
    ///
    /// Arguments passed to `map_or` are eagerly evaluated; if you are passing
    /// the result of a function call, it is recommended to use [`map_or_else`],
    /// which is lazily evaluated.
    ///
    /// [`map_or_else`]: OptionTrait::map_or_else
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Some("foo").map_or(42, |v: ByteArray| v.len()), 3);
    ///
    /// let x: Option<ByteArray> = None;
    /// assert_eq!(x.map_or(42, |v: ByteArray| v.len()), 42);
    /// ```
    #[must_use]
    fn map_or<U, F, +Drop<U>, +Drop<F>, +core::ops::FnOnce<F, (T,)>[Output: U]>(
        self: Option<T>, default: U, f: F,
    ) -> U;

    /// Computes a default function result (if none), or
    /// applies a different function to the contained value (if any).
    ///
    /// # Basic examples
    ///
    /// ```
    /// let k = 21;
    ///
    /// let x = Some("foo");
    /// assert_eq!(x.map_or_else( || 2 * k, |v: ByteArray| v.len()), 3);
    ///
    /// let x: Option<ByteArray> = None;
    /// assert_eq!(x.map_or_else( || 2 * k, |v: ByteArray| v.len()), 42);
    /// ```
    fn map_or_else<
        U,
        D,
        F,
        +Drop<U>,
        +Drop<D>,
        +Drop<F>,
        +core::ops::FnOnce<D, ()>[Output: U],
        +core::ops::FnOnce<F, (T,)>[Output: U],
    >(
        self: Option<T>, default: D, f: F,
    ) -> U;

    /// Takes the value out of the option, leaving a [`None`] in its place.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut x = Some(2);
    /// let y = x.take();
    /// assert_eq!(x, None);
    /// assert_eq!(y, Some(2));
    ///
    /// let mut x: Option<u32> = None;
    /// let y = x.take();
    /// assert_eq!(x, None);
    /// assert_eq!(y, None);
    /// ```
    fn take(ref self: Option<T>) -> Option<T>;

    /// Returns [`None`] if the option is [`None`], otherwise calls `predicate`
    /// with the wrapped value and returns:
    ///
    /// - [`Some(t)`] if `predicate` returns `true` (where `t` is the wrapped
    ///   value), and
    /// - [`None`] if `predicate` returns `false`.
    ///
    /// # Example
    ///
    /// ```
    /// let is_even = |n: @u32| -> bool {
    ///     *n % 2 == 0
    /// };
    ///
    /// assert_eq!(None.filter(is_even), None);
    /// assert_eq!(Some(3).filter(is_even), None);
    /// assert_eq!(Some(4).filter(is_even), Some(4));
    /// ```
    fn filter<P, +core::ops::FnOnce<P, (@T,)>[Output: bool], +Destruct<T>, +Destruct<P>>(
        self: Option<T>, predicate: P,
    ) -> Option<T>;

    /// Converts from `Option<Option<T>>` to `Option<T>`.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// let x: Option<Option<u32>> = Some(Some(6));
    /// assert_eq!(Some(6), x.flatten());
    ///
    /// let x: Option<Option<u32>> = Some(None);
    /// assert_eq!(None, x.flatten());
    ///
    /// let x: Option<Option<u32>> = None;
    /// assert_eq!(None, x.flatten());
    /// ```
    ///
    /// Flattening only removes one level of nesting at a time:
    ///
    /// ```
    /// let x: Option<Option<Option<u32>>> = Some(Some(Some(6)));
    /// assert_eq!(Some(Some(6)), x.flatten());
    /// assert_eq!(Some(6), x.flatten().flatten());
    /// ```
    fn flatten(self: Option<Option<T>>) -> Option<T>;
}

pub impl OptionTraitImpl<T> of OptionTrait<T> {
    #[inline(always)]
    const fn expect(self: Option<T>, err: felt252) -> T {
        match self {
            Some(x) => x,
            None => crate::panic_with_felt252(err),
        }
    }

    #[inline(always)]
    const fn unwrap(self: Option<T>) -> T {
        self.expect('Option::unwrap failed.')
    }

    #[inline]
    fn ok_or<E, +Destruct<E>>(self: Option<T>, err: E) -> Result<T, E> {
        match self {
            Some(v) => Ok(v),
            None => Err(err),
        }
    }

    #[inline]
    fn ok_or_else<E, F, +Destruct<E>, +core::ops::FnOnce<F, ()>[Output: E], +Drop<F>>(
        self: Option<T>, err: F,
    ) -> Result<T, E> {
        match self {
            Some(v) => Ok(v),
            None => Err(err()),
        }
    }

    #[inline]
    fn and<U, +Drop<T>, +Drop<U>>(self: Option<T>, optb: Option<U>) -> Option<U> {
        match self {
            Some(_) => optb,
            None => None,
        }
    }

    #[inline]
    fn and_then<U, F, +Drop<F>, +core::ops::FnOnce<F, (T,)>[Output: Option<U>]>(
        self: Option<T>, f: F,
    ) -> Option<U> {
        match self {
            Some(x) => f(x),
            None => None,
        }
    }

    #[inline]
    fn or<+Drop<T>>(self: Option<T>, optb: Option<T>) -> Option<T> {
        match self {
            Some(x) => Some(x),
            None => optb,
        }
    }

    #[inline]
    fn or_else<F, +Drop<F>, +core::ops::FnOnce<F, ()>[Output: Option<T>]>(
        self: Option<T>, f: F,
    ) -> Option<T> {
        match self {
            Some(x) => Some(x),
            None => f(),
        }
    }

    #[inline]
    fn xor<+Drop<T>>(self: Option<T>, optb: Option<T>) -> Option<T> {
        match (self, optb) {
            (Some(x), None) => Some(x),
            (None, Some(x)) => Some(x),
            _ => None,
        }
    }

    #[inline]
    fn is_some(self: @Option<T>) -> bool {
        match self {
            Some(_) => true,
            None => false,
        }
    }

    #[inline]
    fn is_some_and<F, +Drop<F>, +core::ops::FnOnce<F, (T,)>[Output: bool]>(
        self: Option<T>, f: F,
    ) -> bool {
        match self {
            None => false,
            Some(x) => f(x),
        }
    }

    #[inline]
    fn is_none(self: @Option<T>) -> bool {
        match self {
            Some(_) => false,
            None => true,
        }
    }

    #[inline]
    fn is_none_or<F, +Drop<F>, +core::ops::FnOnce<F, (T,)>[Output: bool]>(
        self: Option<T>, f: F,
    ) -> bool {
        match self {
            Some(x) => f(x),
            None => true,
        }
    }

    #[inline]
    const fn unwrap_or<+Destruct<T>>(self: Option<T>, default: T) -> T {
        match self {
            Some(x) => x,
            None => default,
        }
    }

    #[inline]
    fn unwrap_or_default<+Default<T>>(self: Option<T>) -> T {
        match self {
            Some(x) => x,
            None => Default::default(),
        }
    }

    #[inline]
    fn unwrap_or_else<
        F, +Drop<F>, impl func: core::ops::FnOnce<F, ()>[Output: T], +Drop<func::Output>,
    >(
        self: Option<T>, f: F,
    ) -> T {
        match self {
            Some(x) => x,
            None => f(),
        }
    }

    #[inline]
    fn map<U, F, +Destruct<F>, +core::ops::FnOnce<F, (T,)>[Output: U]>(
        self: Option<T>, f: F,
    ) -> Option<U> {
        match self {
            Some(x) => Some(f(x)),
            None => None,
        }
    }

    #[inline]
    fn map_or<U, F, +Drop<U>, +Drop<F>, +core::ops::FnOnce<F, (T,)>[Output: U]>(
        self: Option<T>, default: U, f: F,
    ) -> U {
        match self {
            Some(x) => f(x),
            None => default,
        }
    }

    #[inline]
    fn map_or_else<
        U,
        D,
        F,
        +Drop<U>,
        +Drop<D>,
        +Drop<F>,
        +core::ops::FnOnce<D, ()>[Output: U],
        +core::ops::FnOnce<F, (T,)>[Output: U],
    >(
        self: Option<T>, default: D, f: F,
    ) -> U {
        match self {
            Some(x) => f(x),
            None => default(),
        }
    }

    #[inline]
    fn take(ref self: Option<T>) -> Option<T> {
        let value = self;
        self = None;
        value
    }

    #[inline]
    fn filter<P, +core::ops::FnOnce<P, (@T,)>[Output: bool], +Destruct<T>, +Destruct<P>>(
        self: Option<T>, predicate: P,
    ) -> Option<T> {
        if let Some(value) = self {
            if predicate(@value) {
                return Some(value);
            }
        }

        None
    }

    #[inline]
    fn flatten(self: Option<Option<T>>) -> Option<T> {
        match self {
            Some(x) => x,
            None => None,
        }
    }
}

impl TIntoOption<T> of Into<T, Option<T>> {
    /// Moves `self` into a new [`Some`].
    ///
    /// # Examples
    ///
    /// ```
    /// let o: Option<u8> = 67_u8.into()
    /// assert_eq!(Some(67), o);
    /// ```
    #[inline]
    fn into(self: T) -> Option<T> {
        Some(self)
    }
}


/// An iterator over the value in the [`Some`] variant of an [`Option`].
///
/// The iterator yields one value if the [`Option`] is a [`Some`], otherwise none.
///
/// This struct is created by the [`into_iter`] method on [`Option`] (provided by the
/// [`IntoIterator`] trait).
///
/// [`into_iter`]: IntoIterator::into_iter
#[derive(Drop)]
pub struct OptionIter<T> {
    inner: Option<T>,
}

impl OptionIterator<T> of crate::iter::Iterator<OptionIter<T>> {
    type Item = T;
    fn next(ref self: OptionIter<T>) -> Option<T> {
        let item = self.inner;
        self.inner = None;
        item
    }
}

impl OptionIntoIterator<T> of crate::iter::IntoIterator<Option<T>> {
    type IntoIter = OptionIter<T>;
    #[inline]
    fn into_iter(self: Option<T>) -> Self::IntoIter {
        OptionIter { inner: self }
    }
}
