// Calculates fib...
func fib(a: felt, b: felt, n: felt) -> felt {
    match n {
        0 => { a }
        _ => { fib(b, felt_add(a, b), felt_add(n, -1)) }
    }
}
