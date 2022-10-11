// Calculates fib, but all variables are boxes.
func fib(a: Box::<felt>, b: Box::<felt>, n: Box::<felt>) -> Box::<felt> {
    let unboxed_n = unbox::<felt>(n);
    match unboxed_n {
        0 => a,
        _ => {
            fib(
                b,
                into_box::<felt>(unbox::<felt>(a) + unbox::<felt>(b)),
                into_box::<felt>(unboxed_n - 1),
            )
        },
    }
}
