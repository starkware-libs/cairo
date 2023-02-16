// Calculates fib...
fn fib(a: felt, b: felt, n: felt) -> felt implicits(RangeCheck, GasBuiltin) {
    match get_gas() {
        Option::Some(_) => {},
        Option::None(_) => {
            let mut data = queue_new::<felt>();
            queue_append::<felt>(ref data, 'OOG');
            panic(data);
        },
    }
    match n {
        0 => a,
        _ => fib(b, a + b, n - 1),
    }
}
