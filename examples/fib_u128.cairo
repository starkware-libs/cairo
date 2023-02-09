// Calculates fib...
fn fib(a: u128, b: u128, n: u128) -> u128 implicits(RangeCheck) {
    if n == 0_u128 {
        a
    } else {
        fib(b, a + b, n - 1_u128)
    }
}
