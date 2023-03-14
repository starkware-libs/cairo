// Calculates fib, but all variables are boxes.
fn fib(a: Box::<felt252>, b: Box::<felt252>, n: Box::<felt252>) -> Box::<felt252> {
    let unboxed_n = unbox::<felt252>(n);
    if unboxed_n == 0 {
        a
    } else {
        fib(
            b,
            into_box::<felt252>(unbox::<felt252>(a) + unbox::<felt252>(b)),
            into_box::<felt252>(unboxed_n - 1),
        )
    }
}
