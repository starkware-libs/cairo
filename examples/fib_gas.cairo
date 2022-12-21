// Calculates fib...
func fib(a: felt, b: felt, n: felt) -> felt implicits(RangeCheck, GasBuiltin) {
    match get_gas() {
        Option::Some(x) => {
        },
        Option::None(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(data, 'OOG');
            panic(data);
        },
    }
    match n {
        0 => a,
        _ => fib(b, a + b, n - 1),
    }
}
