// Calculates fib...
struct FibResult { value: felt, index: felt, nothing: () }
impl FibResultCopy of Copy::<FibResult>;
impl FibResultDrop of Drop::<FibResult>;

func fib(a: felt, b: felt, n: felt) -> FibResult {
    match n {
        0 => FibResult { nothing: (), value: a, index: 0 },
        _ => {
            let FibResult { value, nothing, index } = fib(b, a + b, n - 1);
            FibResult { value: value, nothing: (), index: index + 1 }
        },
    }
}
