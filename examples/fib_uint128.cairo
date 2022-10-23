// Calculates fib...
// TODO(orizi): Switch all matches to `?` usages.
// TODO(orizi): Make `rc` implicit.
func fib(ref rc: RangeCheck, a: uint128, b: uint128, n: uint128) -> Option::<uint128> {
    // TODO(orizi): Use match on uint128 when supported.
    match uint128_to_felt(n) {
        0 => Option::<uint128>::Some(a),
        _ => {
            let new_b = match uint128_add(rc, a, b) {
                Option::Some (t) => t,
                Option::None (_) => {
                    return Option::<uint128>::None(());
                },
            };
            // TODO(orizi): Use uint128 literal when supported.
            let one = match uint128_from_felt(rc, 1) {
                Option::Some (t) => t,
                Option::None (_) => {
                    return Option::<uint128>::None(());
                },
            };
            let new_n = match uint128_sub(rc, n, one) {
                Option::Some (t) => t,
                Option::None (_) => {
                    return Option::<uint128>::None(());
                },
            };
            fib(rc, b, new_b, new_n)
        },
    }
}
