// Calculates fib...
func fib(a: felt, b: felt, n: felt) -> felt nopanic {
    match n {
        0 => a,
        _ => fib(b, a + b, n - 1),
    }
}
