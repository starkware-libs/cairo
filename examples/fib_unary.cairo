mod inner { func fib_inner(a: felt, b: felt, n: felt) -> felt {
    if n != 0 {
        fib_inner(b, a + b, n - 1)
    } else {
        a
    }
}
}

use inner::fib_inner;

func fib(n: felt) -> felt {
    -inner::fib_inner(-1, -1, n)
}
