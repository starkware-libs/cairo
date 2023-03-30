// Calculates fib...
fn fib(a: felt252, b: felt252, n: felt252) -> (felt252, felt252) {
    match n {
        0 => (a, 0),
        _ => {
            let (v, count) = fib(b, a + b, n - 1);
            (v, count + 1)
        },
    }
}
