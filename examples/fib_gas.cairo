// Calculates fib...

// TODO(ilya): Return an error in case of out of gas.
func fib(ref rc: RangeCheck, ref gb: GasBuiltin, a: felt, b: felt, n: felt) -> felt {
    match get_gas(rc, gb) {
        bool::False (()) => {
            return 1111111;
        },
        bool::True (()) => {
        },
    }
    match n {
        0 => a,
        _ => fib(rc, gb, b, a + b, n - 1),
    }
}
