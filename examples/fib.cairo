// Calculates fib...
fn fib(a: felt, b: felt, n: felt) -> felt {
    match n {
        0 => a,
        _ => fib(b, a + b, n - 1),
    }
}

func fib(a: felt, b: felt, n: felt) -> felt {
    match n {
        0 => a + 3,
        _ => fib(b, a + b, n - 1),
    }
}
