use array::ArrayTrait;
enum Result<T, E> {
    Ok: T,
    Err: E,
}
trait ResultTrait<T, E> {
    /// If `val` is `Result::Ok(x)`, returns `x`. Otherwise, panics with `err`.
    fn expect<impl EDrop: Drop<E>>(self: Result<T, E>, err: felt252) -> T;
    /// If `val` is `Result::Ok(x)`, returns `x`. Otherwise, panics.
    fn unwrap<impl EDrop: Drop<E>>(self: Result<T, E>) -> T;
    /// If `val` is `Result::Err(x)`, returns `x`. Otherwise, panics with `err`.
    fn expect_err<impl TDrop: Drop<T>>(self: Result<T, E>, err: felt252) -> E;
    /// If `val` is `Result::Err(x)`, returns `x`. Otherwise, panics.
    fn unwrap_err<impl TDrop: Drop<T>>(self: Result<T, E>) -> E;
    /// Returns `true` if the `Result` is `Result::Ok`.
    fn is_ok(self: @Result<T, E>) -> bool;
    /// Returns `true` if the `Result` is `Result::Err`.
    fn is_err(self: @Result<T, E>) -> bool;
    /// Returns `true` if the `Result` is `Result::Ok`, and consumes the value.
    fn into_is_err<impl TDrop: Drop<T>, impl EDrop: Drop<E>>(self: Result<T, E>) -> bool;
    /// Returns `true` if the `Result` is `Result::Err`, and consumes the value.
    fn into_is_ok<impl TDrop: Drop<T>, impl EDrop: Drop<E>>(self: Result<T, E>) -> bool;
}
impl ResultTraitImpl<T, E> of ResultTrait<T, E> {
    fn expect<impl EDrop: Drop<E>>(self: Result<T, E>, err: felt252) -> T {
        match self {
            Result::Ok(x) => x,
            Result::Err(_) => panic_with_felt252(err),
        }
    }
    fn unwrap<impl EDrop: Drop<E>>(self: Result<T, E>) -> T {
        self.expect('Result::unwrap failed.')
    }
    fn expect_err<impl TDrop: Drop<T>>(self: Result<T, E>, err: felt252) -> E {
        match self {
            Result::Ok(_) => panic_with_felt252(err),
            Result::Err(x) => x,
        }
    }
    fn unwrap_err<impl TDrop: Drop<T>>(self: Result<T, E>) -> E {
        self.expect_err('Result::unwrap_err failed.')
    }
    #[inline]
    fn is_ok(self: @Result<T, E>) -> bool {
        match self {
            Result::Ok(_) => true,
            Result::Err(_) => false,
        }
    }
    #[inline]
    fn is_err(self: @Result<T, E>) -> bool {
        match self {
            Result::Ok(_) => false,
            Result::Err(_) => true,
        }
    }
    #[inline]
    fn into_is_err<impl TDrop: Drop<T>, impl EDrop: Drop<E>>(self: Result<T, E>) -> bool {
        match self {
            Result::Ok(_) => false,
            Result::Err(_) => true,
        }
    }
    #[inline]
    fn into_is_ok<impl TDrop: Drop<T>, impl EDrop: Drop<E>>(self: Result<T, E>) -> bool {
        match self {
            Result::Ok(_) => true,
            Result::Err(_) => false,
        }
    }
}

// Impls for generic types.
impl ResultCopy<T, E, impl TCopy: Copy<T>, impl ECopy: Copy<E>> of Copy<Result<T, E>>;
impl ResultDrop<T, E, impl TDrop: Drop<T>, impl EDrop: Drop<E>> of Drop<Result<T, E>>;
