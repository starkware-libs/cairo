// Calculates fib...
// TODO(orizi): Switch all matches to `?` usages.
func fib(a: uint128, b: uint128, n: uint128) -> Option::<uint128> implicits (rc: RangeCheck) {
    // TODO(orizi): Use match on uint128 when supported.
    match uint128_to_felt(n) {
        0 => Option::<uint128>::Some(a),
        _ => {
            // TODO(orizi): Use uint128 literal when supported.
            let r = fib(b, (a + b)?, (n - uint128_from_felt(1)?)?)?;
            Option::<uint128>::Some(r)
        },
    }
}
