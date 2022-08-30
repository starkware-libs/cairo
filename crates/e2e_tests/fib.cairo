// Calculates fib...
func fib(a: felt, b: felt, n: felt) -> felt {
    match n {
        0 => { return a; }
        _ => { return fib(b, a+b, n-1); }
    }
}
