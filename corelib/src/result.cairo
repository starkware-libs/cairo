use array::ArrayTrait;
use serde::Serde;
use array::SpanTrait;

#[derive(Copy, Drop, Serde, PartialEq)]
enum Result<T, E> {
    Ok: T,
    Err: E,
}

#[generate_trait]
impl ResultTraitImpl<T, E> of ResultTrait<T, E> {
    /// If `val` is `Result::Ok(x)`, returns `x`. Otherwise, panics with `err`.
    fn expect<+Drop<E>>(self: Result<T, E>, err: felt252) -> T {
        match self {
            Result::Ok(x) => x,
            Result::Err(_) => panic_with_felt252(err),
        }
    }
    /// If `val` is `Result::Ok(x)`, returns `x`. Otherwise, panics.
    fn unwrap<+Drop<E>>(self: Result<T, E>) -> T {
        self.expect('Result::unwrap failed.')
    }
    /// If `val` is `Result::Err(x)`, returns `x`. Otherwise, panics with `err`.
    fn expect_err<+Drop<T>>(self: Result<T, E>, err: felt252) -> E {
        match self {
            Result::Ok(_) => panic_with_felt252(err),
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
}
