// Returns an array of size n with fib values.
func fib(a: felt, b: felt, n: felt) -> Array::<felt> {
    helper(a, b, n, array_new::<felt>());
}

func helper(a: felt, b: felt, n: felt, arr: Array::<felt>) -> Array::<felt> {
    match arr.len - n {
        0 => {
            arr
        },
        _ => {
            helper(b, a + b, n, array_push::<felt>(arr, a))
        },
    }
}
