// Returns an array of size n with the values of the Fibonacci sequence.
func fib(n: uint128) -> Array::<felt> {
    let mut arr = array_new::<felt>();
    array_append::<felt>(arr, 1);
    array_append::<felt>(arr, 1);
    fib_inner(n, arr)
}

func unchecked_array_at(ref arr: Array::<felt>, idx: uint128) -> felt {
    match array_at::<felt>(arr, idx) {
        Option::Some(v) => v,
        Option::None(()) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(data, 1);
            panic(data)
    } }
}

func fib_inner(n: uint128, mut arr: Array::<felt>) -> Array::<felt> {
    let length = array_len::<felt>(arr);
    if n >= length {
        return arr;
    }
    array_append::<felt>(
        arr,
        unchecked_array_at(arr, length - 1_uint128) + unchecked_array_at(arr, length - 2_uint128)
    );
    fib_inner(n, arr)
}
