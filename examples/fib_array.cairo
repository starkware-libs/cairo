// Returns an array of size n with the values of the Fibonacci sequence, the length of the array,
// and the value of the last element.
fn fib(n: u128) -> (Array::<felt>, felt, u128) {
    let mut arr = array_new();
    array_append(ref arr, 1);
    array_append(ref arr, 1);
    let mut arr = fib_inner(:n, :arr);
    let len = my_array_len(ref arr);
    let last = unchecked_array_at(ref arr, len - 1_u128);
    return (arr, last, len);
}

fn fib_inner(n: u128, mut arr: Array::<felt>) -> Array::<felt> {
    let length = my_array_len(ref arr);
    if n <= length {
        return arr;
    }
    array_append(
        ref arr,
        unchecked_array_at(ref arr, length - 1_u128) + unchecked_array_at(ref arr, length - 2_u128)
    );
    fib_inner(:n, :arr)
}

// TODO(orizi): Remove when a panicable `array_at` is introduced.
fn unchecked_array_at(ref arr: Array::<felt>, idx: u128) -> felt {
    match array_at(ref arr, idx) {
        Option::Some(v) => v,
        Option::None(()) => {
            let mut data = array_new();
            array_append(ref data, 1);
            panic(data)
        },
    }
}

fn my_array_len<T>(ref arr: Array::<T>) -> u128 {
    array_len(ref arr)
}
