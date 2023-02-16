// Calculates H(...H(H(0, 1), ..., n))...) where H is the Pedersen hash function.
fn hash_chain(n: felt) -> felt {
    if n == 0 {
        return 0;
    }

    match get_gas_all(get_builtin_costs()) {
        Option::Some(x) => {},
        Option::None(x) => {
            let mut data = queue_new::<felt>();
            queue_append::<felt>(ref data, 'Out of gas');
            panic(data);
        },
    }

    pedersen(hash_chain(n - 1), n)
}
