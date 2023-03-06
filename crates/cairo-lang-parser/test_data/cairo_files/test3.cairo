fn main() -> Option<felt> {
    fib(1, 1, 13)
}

/// Calculates fib...
fn fib(a: felt, b: felt, n: felt) -> Option<felt> {
    gas::get_gas()?;
    match n {
        0 => Option::Some(a),
        _ => fib(b, a + b, n - 1),
    }
}
