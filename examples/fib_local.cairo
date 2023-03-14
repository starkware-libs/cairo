fn fib(n: felt252) -> felt252 {
    if n == 0 {
        1
    } else if n == 1 {
        1
    } else {
        fib(n - 1) + fib(n - 2)
    }
}
