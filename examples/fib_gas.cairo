// Calculates fib...
fn fib(a: felt252, b: felt252, n: felt252) -> felt252 implicits(RangeCheck, GasBuiltin) {
    match gas::get_gas() {
        Option::Some(_) => {},
        Option::None(_) => {
            let mut data = array_new::<felt252>();
            array_append::<felt252>(ref data, 'OOG');
            panic(data);
        },
    }
    match n {
        0 => a,
        _ => fib(b, a + b, n - 1),
    }
}
