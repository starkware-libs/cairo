func fib(n: felt) -> felt {
    -_fib(-1, -1, n)
}

func _fib(a: felt, b: felt, n: felt) -> felt {
    // TODO(alont): swap this `if` once !(n == 0) is supported.
    if n == 0 {
        a
    } else {
        _fib(b, a + b, n - 1)
    }
}
