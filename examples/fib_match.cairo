// Calculates fib...
fn fib(n: felt252) -> felt252 {
    match n {
        0 => 1,
        1 => 1,
        2 => 2,
        3 => 3,
        4 => 5,
        _ => { 5 * fib(n - 4) + 3 * fib(n - 5) }
    }
}
