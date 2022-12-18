func fib(n: felt) -> felt {
    if n == 0 {
        'a'
    } else if n == 1 {
        'b'
    } else {
        fib(n - 1) + fib(n - 2)
    }
}
