use array::ArrayTrait;
enum Result<T, E> {
    Ok: T,
    Err: E,
}
trait ResultTrait<T, E> {
    /// If `val` is `Result::Ok(x)`, returns `x`. Otherwise, panics with `err`.
    fn expect(self: Result::<T, E>, err: felt) -> T;
    /// If `val` is `Result::Ok(x)`, returns `x`. Otherwise, panics.
    fn unwrap(self: Result::<T, E>) -> T;
    /// If `val` is `Result::Err(x)`, returns `x`. Otherwise, panics with `err`.
    fn expect_err(self: Result::<T, E>, err: felt) -> E;
    /// If `val` is `Result::Err(x)`, returns `x`. Otherwise, panics.
    fn unwrap_err(self: Result::<T, E>) -> E;
    /// Returns `true` if the `Result` is `Result::Ok`.
    fn is_ok(self: Result::<T, E>) -> bool;
    /// Returns `true` if the `Result` is `Result::Err`.
    fn is_err(self: Result::<T, E>) -> bool;
}
impl ResultTraitImpl<T, E> of ResultTrait::<T, E> {
    fn expect(self: Result::<T, E>, err: felt) -> T {
        match self {
            Result::Ok(x) => x,
            Result::Err(_) => {
                let mut data = ArrayTrait::new();
                data.append(err)
                panic(data)
            },
        }
    }
    fn unwrap(self: Result::<T, E>) -> T {
        self.expect('Result::unwrap failed.')
    }
    fn expect_err(self: Result::<T, E>, err: felt) -> E {
        match self {
            Result::Ok(_) => {
                let mut data = ArrayTrait::new();
                data.append(err)
                panic(data)
            },
            Result::Err(x) => x,
        }
    }
    fn unwrap_err(self: Result::<T, E>) -> E {
        self.expect_err('Result::unwrap_err failed.')
    }
    fn is_ok(self: Result::<T, E>) -> bool {
        match self {
            Result::Ok(_) => true,
            Result::Err(_) => false,
        }
    }
    fn is_err(self: Result::<T, E>) -> bool {
        match self {
            Result::Ok(_) => false,
            Result::Err(_) => true,
        }
    }
}
