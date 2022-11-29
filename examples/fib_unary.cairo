func fib(n: felt) -> felt {
    -fib_inner(-1, -1, n)
}
func fib_inner(a: felt, b: felt, n: felt) -> felt {
    // TODO(alont): change the condition to !(n == 0) once it's supported.
    if n != 0 {
        fib_inner(b, a + b, n - 1)
    } else {
        a
    }
}
