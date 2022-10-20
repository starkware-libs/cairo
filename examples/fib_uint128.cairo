// Calculates fib...
// TODO(orizi): Switch all matches to `?` usages.
func fib(rc: RangeCheck, a: uint128, b: uint128, n: uint128) -> (RangeCheck, Option::<uint128>) {
    // TODO(orizi): Use match on uin128 when supported.
    match uint128_to_felt(n) {
        0 => (rc, Option::<uint128>::Some(a)),
        _ => {
            let (rc, new_b) = uint128_add(rc, a, b);
            let new_b = match new_b {
                Option::Some (v) => v,
                Option::None (_) => {
                    return (rc, Option::<uint128>::None(()));
            } };
            // TODO(orizi): Use uint128 literal when supported.
            let (rc, one) = uint128_from_felt(rc, 1);
            let one = match one {
                Option::Some (v) => v,
                Option::None (_) => {
                    return (rc, Option::<uint128>::None(()));
            } };
            let (rc, new_n) = uint128_sub(rc, n, one);
            let new_n = match new_n {
                Option::Some (v) => v,
                Option::None (_) => {
                    return (rc, Option::<uint128>::None(()));
            } };
            fib(rc, b, new_b, new_n)
    } }
}
