// Calculates fib, but all variables are references.
func fib(a: Ref::<felt>, b: Ref::<felt>, n: Ref::<felt>) -> Ref::<felt> {
    let n_deref = deref::<felt>(n);
    match n_deref {
        0 => {
            a
        },
        _ => {
            fib(
                b,
                into_ref::<felt>(deref::<felt>(a) + deref::<felt>(b)),
                into_ref::<felt>(n_deref - 1),
            )
        },
    }
}
