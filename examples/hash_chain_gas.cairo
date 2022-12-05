// Calculates H(...H(H(0, 1), ..., n))...) where H is the Pedersen hash function.
func hash_chain(n: felt, pedersen_cost: PedersenBuiltinCost) -> felt {
    if n == 0 {
        return 0;
    }

    match get_gas() {
        Option::Some(x) => {
        },
        Option::None(x) => {
            let data = array_new::<felt>();
            array_append::<felt>(data, 1);
            panic(data);
        },
    }

    match pedersen_get_gas(pedersen_cost) {
        Option::Some(x) => {
        },
        Option::None(x) => {
            let data = array_new::<felt>();
            array_append::<felt>(data, 1);
            panic(data);
        },
    }

    pedersen(hash_chain(n - 1, pedersen_cost), n)
}
