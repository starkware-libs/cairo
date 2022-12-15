// Calculates fib...
// TODO(orizi): Switch all matches to `?` usages.
func fib(a: uint128, b: uint128, n: uint128) -> uint128 implicits(RangeCheck) {
    // TODO(orizi): Use match on uint128 when supported.
    match uint128_to_felt(n) {
        0 => a,
        _ => {
            fib(b, a + b, n - 1_uint128)
        },
    }
}
