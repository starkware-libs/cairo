use array::ArrayTrait;

// Calculates fib...
fn fib(a: felt252, b: felt252, n: felt252) -> felt252 implicits(RangeCheck, GasBuiltin) {
    match gas::withdraw_gas() {
        Option::Some(_) => {},
        Option::None(_) => {
            let mut data = ArrayTrait::new();
            data.append('OOG');
            panic(data);
        },
    }
    match n {
        0 => a,
        _ => fib(b, a + b, n - 1),
    }
}
