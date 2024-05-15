use core::array::ArrayTrait;
use core::serde::Serde;
use core::array::SpanTrait;

#[must_use]
#[derive(Copy, Drop, Debug, Serde, PartialEq)]
pub enum Result<T, E> {
    Ok: T,
    Err: E,
}

#[generate_trait]
pub impl ResultTraitImpl<T, E> of ResultTrait<T, E> {
    /// If `val` is `Result::Ok(x)`, returns `x`. Otherwise, panics with `err`.
    fn expect<+Drop<E>>(self: Result<T, E>, err: felt252) -> T {
        match self {
            Result::Ok(x) => x,
            Result::Err(_) => core::panic_with_felt252(err),
        }
    }
    /// If `val` is `Result::Ok(x)`, returns `x`. Otherwise, panics.
    fn unwrap<+Drop<E>>(self: Result<T, E>) -> T {
        self.expect('Result::unwrap failed.')
    }
    /// If `val` is `Result::Ok(x)`, returns `x`. Otherwise, returns `default`.
    fn unwrap_or<+Drop<T>, +Drop<E>>(self: Result<T, E>, default: T) -> T {
        match self {
            Result::Ok(x) => x,
            Result::Err(_) => default,
        }
    }
    /// If `val` is `Result::Ok(x)`, returns `x`.
    /// Otherwise returns `Default::<T>::default()`.
    fn unwrap_or_default<+Drop<E>, +Default<T>>(self: Result<T, E>) -> T {
        match self {
            Result::Ok(x) => x,
            Result::Err(_) => Default::default(),
        }
    }

    /// If `val` is `Result::Err(x)`, returns `x`. Otherwise, panics with `err`.
    fn expect_err<+Drop<T>>(self: Result<T, E>, err: felt252) -> E {
        match self {
            Result::Ok(_) => core::panic_with_felt252(err),
            Result::Err(x) => x,
        }
    }
    /// If `val` is `Result::Err(x)`, returns `x`. Otherwise, panics.
    fn unwrap_err<+Drop<T>>(self: Result<T, E>) -> E {
        self.expect_err('Result::unwrap_err failed.')
    }
    /// Returns `true` if the `Result` is `Result::Ok`.
    #[inline]
    fn is_ok(self: @Result<T, E>) -> bool {
        match self {
            Result::Ok(_) => true,
            Result::Err(_) => false,
        }
    }
    /// Returns `true` if the `Result` is `Result::Err`.
    #[inline]
    fn is_err(self: @Result<T, E>) -> bool {
        match self {
            Result::Ok(_) => false,
            Result::Err(_) => true,
        }
    }
    /// Returns `true` if the `Result` is `Result::Ok`, and consumes the value.
    #[inline]
    fn into_is_err<+Drop<T>, +Drop<E>>(self: Result<T, E>) -> bool {
        match self {
            Result::Ok(_) => false,
            Result::Err(_) => true,
        }
    }
    /// Returns `true` if the `Result` is `Result::Err`, and consumes the value.
    #[inline]
    fn into_is_ok<+Drop<T>, +Drop<E>>(self: Result<T, E>) -> bool {
        match self {
            Result::Ok(_) => true,
            Result::Err(_) => false,
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
    /// assert_eq!(x.ok(), Option::Some(2));
    ///
    /// let x: Result<u32, ByteArray> = Result::Err("Nothing here");
    /// assert!(x.ok().is_none());
    /// ```
    fn ok<+Drop<T>, +Drop<E>>(self: Result<T, E>) -> Option<T> {
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
    /// assert_eq!(x.err(), Option::Some("Nothing here"));
    ///
    /// let x: Result<u32, ByteArray> = Result::Ok(2);
    /// assert!(x.err().is_none());
    /// ```
    fn err<+Drop<T>, +Drop<E>>(self: Result<T, E>) -> Option<E> {
        match self {
            Result::Ok(_) => Option::None,
            Result::Err(x) => Option::Some(x),
        }
    }
}
