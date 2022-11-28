// Calculates fib...
// TODO(orizi): Switch all matches to `?` usages.
func fib(a: uint128, b: uint128, n: uint128) -> uint128 implicits (rc: RangeCheck) {
    // TODO(orizi): Use match on uint128 when supported.
    match uint128_to_felt(n) {
        0 => a,
        _ => {
            // TODO(orizi): Use uint128 literal when supported.
            fib(b, a + b, n - integer::uint128_from_felt_panicable(1))
        },
    }
}
