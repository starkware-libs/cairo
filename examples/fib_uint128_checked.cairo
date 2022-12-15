// Calculates fib...
// TODO(orizi): Switch all matches to `?` usages.
func fib(a: uint128, b: uint128, n: uint128) -> Option::<uint128> implicits(RangeCheck) nopanic {
    // TODO(orizi): Use match on uint128 when supported.
    match uint128_to_felt(n) {
        0 => Option::<uint128>::Some(a),
        _ => {
            let r = fib(
                b, integer::uint128_checked_add(a, b)?, integer::uint128_checked_sub(n, 1_uint128)?
            )?;
            Option::<uint128>::Some(r)
        },
    }
}
