// Calculates fib, but all variables are references.
func fib(a: Ref::<felt>, b: Ref::<felt>, n: Ref::<felt>) -> Ref::<felt> {
    match n {
        0 => {
            a
        },
        _ => {
            fib(
                b,
                into_ref::<felt>(deref::<felt>(a) + deref::<felt>(b)),
                into_ref::<felt>(deref::<felt>(n) - 1),
            )
        },
    }
}
