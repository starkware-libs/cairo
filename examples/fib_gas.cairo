// Calculates fib...

// TODO(orizi): Replace with Option when generic enums are fully supported.
enum OptionFelt { Some: felt, None: (), }

func fib(a: felt, b: felt, n: felt) -> OptionFelt implicits (rc: RangeCheck, gb: GasBuiltin) {
    match get_gas() {
        GetGasResult::Success (()) => {
        },
        GetGasResult::Failure (()) => {
            return OptionFelt::None(());
        },
    }
    match n {
        0 => OptionFelt::Some(a),
        _ => fib(b, a + b, n - 1),
    }
}
