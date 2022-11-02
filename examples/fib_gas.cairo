// Calculates fib...

// TODO(ilya): Return an error in case of out of gas.
func fib(a: felt, b: felt, n: felt) -> felt implicits (rc: RangeCheck, gb: GasBuiltin) {
    match get_gas() {
        GetGasResult::Success (()) => {
        },
        GetGasResult::Failure (()) => {
            return 1111111;
        },
    }
    match n {
        0 => a,
        _ => fib(b, a + b, n - 1),
    }
}
