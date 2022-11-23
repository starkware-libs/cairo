// Returns an array of size n with the values of the Fibonacci sequence.
func fib(n: felt) -> Array::<felt> {
    fib_inner(1, 1, n, array_new::<felt>())
}

func fib_inner(a: felt, b: felt, remaining: felt, arr: Array::<felt>) -> Array::<felt> {
    if remaining == 0 {
        return arr;
    }

    array_append::<felt>(arr, a);
    fib_inner(b, a + b, remaining - 1, arr)
}
