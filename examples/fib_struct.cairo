// Calculates fib...
#[derive(Copy, Drop)]
struct FibResult {
    value: felt252,
    index: felt252,
    nothing: ()
}

fn fib(a: felt252, b: felt252, n: felt252) -> FibResult {
    match n {
        0 => FibResult { nothing: (), value: a, index: 0 },
        _ => {
            let r = fib(b, a + b, n - 1);
            FibResult { value: r.value, nothing: (), index: r.index + 1 }
        },
    }
}
