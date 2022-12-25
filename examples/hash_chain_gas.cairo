// Calculates H(...H(H(0, 1), ..., n))...) where H is the Pedersen hash function.
fn hash_chain(n: felt, costs: BuiltinCosts) -> felt {
    if n == 0 {
        return 0;
    }

    match get_gas_all(costs) {
        Option::Some(x) => {
        },
        Option::None(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(data, 'OOG');
            panic(data);
        },
    }

    pedersen(hash_chain(n - 1, costs), n)
}
