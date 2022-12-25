// Calculates fib...
// TODO(orizi): Switch all matches to `?` usages.
fn fib(a: u128, b: u128, n: u128) -> u128 implicits(RangeCheck) {
    // TODO(orizi): Use match on u128 when supported.
    match u128_to_felt(n) {
        0 => a,
        _ => {
            fib(b, a + b, n - 1_u128)
        },
    }
}
