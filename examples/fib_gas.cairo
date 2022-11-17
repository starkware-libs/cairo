// Calculates fib...
func fib(a: felt, b: felt, n: felt) -> Option::<felt> implicits (rc: RangeCheck, gb: GasBuiltin) {
    get_gas()?;
    match n {
        0 => Option::<felt>::Some(a),
        _ => fib(b, a + b, n - 1),
    }
}
