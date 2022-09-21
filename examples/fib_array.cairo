// Extends array to be an array of size n with fib values, will return None in cases of failures.
func fib(arr: Array::<felt>, n: felt) -> Option::<Array::<felt>> {
    if arr.len() == n {
        option_from_value::<Array::<felt>>(arr)
    } else {
        array_push::<felt>(
            arr,
            array_get::<felt>(arr, arr.len() - 2)? + array_get::<felt>(arr, arr.len() - 1)?,
        )
    }
}
