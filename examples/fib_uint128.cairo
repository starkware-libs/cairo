// Calculates fib...
// TODO(orizi): Switch all matches to `?` usages.
// TODO(orizi): Make `rc` mut and then implicit.
func fib(rc: RangeCheck, a: uint128, b: uint128, n: uint128) -> (RangeCheck, Option::<uint128>) {
    // TODO(orizi): Use match on uin128 when supported.
    match uint128_to_felt(n) {
        0 => (rc, Option::<uint128>::Some(a)),
        _ => {
            let (rc, new_b) = match uint128_add(rc, a, b) {
                Result::Ok (t) => t,
                Result::Err (rc) => {
                    return (rc, Option::<uint128>::None(()));
                },
            };
            // TODO(orizi): Use uint128 literal when supported.
            let (rc, one) = match uint128_from_felt(rc, 1) {
                Result::Ok (t) => t,
                Result::Err (rc) => {
                    return (rc, Option::<uint128>::None(()));
                },
            };
            let (rc, new_n) = match uint128_sub(rc, n, one) {
                Result::Ok (t) => t,
                Result::Err (rc) => {
                    return (rc, Option::<uint128>::None(()));
                },
            };
            fib(rc, b, new_b, new_n)
        },
    }
}
