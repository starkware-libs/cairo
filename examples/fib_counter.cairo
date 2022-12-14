// Calculates fib...
fn fib(a: felt, b: felt, n: felt) -> (felt, felt) {
    match n {
        0 => (a, 0),
        _ => {
            let (v, count) = fib(b, a + b, n - 1);
            (v, count + 1)
        },
    }
}
