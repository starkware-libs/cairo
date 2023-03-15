// Calculates H(...H(H(0, 1), ..., n))...) where H is the Pedersen hash function.
fn hash_chain(n: felt252) -> felt252 {
    if n == 0 {
        return 0;
    }

    match gas::get_gas_all(get_builtin_costs()) {
        Option::Some(x) => {},
        Option::None(x) => {
            let mut data = array_new::<felt252>();
            array_append::<felt252>(ref data, 'Out of gas');
            panic(data);
        },
    }

    pedersen(hash_chain(n - 1), n)
}
