fn main() -> Option<felt252> {
    fib(1, 1, 13)
}

/// Calculates fib...
fn fib(a: felt252, b: felt252, n: felt252) -> Option<felt252> {
    gas::withdraw_gas()?;
    match n {
        0 => Option::Some(a),
        _ => fib(b, a + b, n - 1),
    }
}
