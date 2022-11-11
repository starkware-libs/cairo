// Calculates fib...
// TODO(orizi): Switch all matches to `?` usages.
func fib(a: uint128, b: uint128, n: uint128) -> Option::<uint128> implicits (rc: RangeCheck) {
    // TODO(orizi): Use match on uint128 when supported.
    match uint128_to_felt(n) {
        0 => Option::<uint128>::Some(a),
        _ => {
            let new_b = match a + b {
                Option::Some (t) => t,
                Option::None (_) => {
                    return Option::<uint128>::None(());
                },
            };
            // TODO(orizi): Use uint128 literal when supported.
            let one = match uint128_from_felt(1) {
                Option::Some (t) => t,
                Option::None (_) => {
                    return Option::<uint128>::None(());
                },
            };
            let new_n = match n - one {
                Option::Some (t) => t,
                Option::None (_) => {
                    return Option::<uint128>::None(());
                },
            };
            fib(b, new_b, new_n)
        },
    }
}
