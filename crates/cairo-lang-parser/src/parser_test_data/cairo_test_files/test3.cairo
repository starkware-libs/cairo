fn main() -> Option<felt252> {
    fib(1, 1, 13)
}

/// Calculates fib...
fn fib(a: felt252, b: felt252, n: felt252) -> Option<felt252> {
    gas::withdraw_gas()?;
    some_macro!();
    match n {
        0 => Some(a),
        _ => fib(b, a + b, n - 1),
    }
}

pub fn fib_tuple(a: felt252, b: felt252, n: felt252) -> (Option<felt252>, Option<felt252>) {
    gas::withdraw_gas()?;
    match n {
        0 => (Some(a), Some(b)),
        _ => fib_tuple(b, a + b, n - 1),
    }
}
