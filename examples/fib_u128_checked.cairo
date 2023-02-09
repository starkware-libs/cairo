// Calculates fib...
fn fib(a: u128, b: u128, n: u128) -> Option::<u128> implicits(RangeCheck) nopanic {
    // TODO(orizi): Use match on u128 when supported.
    match integer::u128_to_felt(n) {
        0 => Option::<u128>::Some(a),
        _ => {
            let r = fib(
                b, integer::u128_checked_add(a, b)?, integer::u128_checked_sub(n, 1_u128)?
            )?;
            Option::<u128>::Some(r)
        },
    }
}
