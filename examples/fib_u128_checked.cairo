// Calculates fib...
fn fib(a: u128, b: u128, n: u128) -> Option<u128> implicits(RangeCheck) {
    match n {
        0 => Some(a),
        _ => {
            let r = fib(
                b,
                core::integer::u128_checked_add(a, b)?,
                core::integer::u128_checked_sub(n, 1_u128)?,
            )?;
            Some(r)
        },
    }
}
