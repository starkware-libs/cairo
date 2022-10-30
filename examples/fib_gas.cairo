// Calculates fib...

// TODO(ilya): Return an error in case of out of gas.
func fib(ref rc: RangeCheck, ref gb: GasBuiltin, a: felt, b: felt, n: felt) -> felt {
    match get_gas(rc, gb) {
        GetGasResult::Success (()) => {
        },
        GetGasResult::Failure (()) => {
            return 1111111;
        },
    }
    match n {
        0 => a,
        _ => fib(rc, gb, b, a + b, n - 1),
    }
}
