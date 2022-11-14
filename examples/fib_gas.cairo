// Calculates fib...
func fib(a: felt, b: felt, n: felt) -> Option::<felt> implicits (rc: RangeCheck, gb: GasBuiltin) {
    match get_gas() {
        GetGasResult::Success (()) => {
        },
        GetGasResult::Failure (()) => {
            return Option::<felt>::None(());
        },
    }
    match n {
        0 => Option::<felt>::Some(a),
        _ => fib(b, a + b, n - 1),
    }
}
