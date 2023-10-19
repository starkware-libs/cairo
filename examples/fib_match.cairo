// Calculates fib...
fn fib(n: felt252) -> felt252 {
    match n {
        0 => 1,
        1 => 1,
        2 => 2,
        3 => 3,
        4 => 5,
        _ => {
            fib(n - 1) + fib(n - 2)
        }
    }
}
