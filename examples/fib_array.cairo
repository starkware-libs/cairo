// Returns an array of size 2n with the values of the Fibonacci sequence, each element appear twice,
// as well as the length of the array.
func fib(n: felt) -> (Array::<felt>, u128) {
    let mut arr = fib_inner(1, 1, n, array_new::<felt>());
    let len = array_len::<felt>(arr);
    (arr, len)
}

func fib_inner(a: felt, b: felt, remaining: felt, mut arr: Array::<felt>) -> Array::<felt> {
    if remaining == 0 {
        return arr;
    }

    array_append::<felt>(arr, a);
    array_append::<felt>(arr, a);
    fib_inner(b, a + b, remaining - 1, arr)
}
