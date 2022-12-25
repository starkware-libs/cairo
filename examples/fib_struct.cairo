// Calculates fib...
#[derive(Copy, Drop)]
struct FibResult { value: felt, index: felt, nothing: () }

fn fib(a: felt, b: felt, n: felt) -> FibResult {
    match n {
        0 => FibResult { nothing: (), value: a, index: 0 },
        _ => {
            let r = fib(b, a + b, n - 1);
            FibResult { value: r.value, nothing: (), index: r.index + 1 }
        },
    }
}
