// Returns an array of size n with the values of the Fibonacci sequence, the length of the array,
// and the value of the last element.
fn fib(n: usize) -> (Array<felt252>, felt252, usize) {
    let mut arr = array![];
    arr.append(1);
    arr.append(1);
    fib_inner(n, ref arr);
    let len = arr.len();
    let last = arr[len - 1_usize];
    return (arr, *last, len);
}

fn fib_inner(n: usize, ref arr: Array<felt252>) {
    let length = arr.len();
    if n <= length {
        return ();
    }
    arr.append(*arr[length - 1_usize] + *arr[length - 2_usize]);
    fib_inner(n, ref arr)
}
