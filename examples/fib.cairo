// Calculates fib...
func fib(a: felt, b: felt, n: felt) -> felt {
    match n {
        0 => a,
        _ => fib(a + b, n - 1),
    }
}
