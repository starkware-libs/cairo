// Calculates H(...H(H(0, 1), ..., n))...) where H is the Pedersen hash function.
func hash_chain(n: felt) -> felt {
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

    match pedersen_get_gas() {
        Option::Some(x) => {
        },
        Option::None(x) => {
            let data = array_new::<felt>();
            array_append::<felt>(data, 1);
            panic(data);
        },
    }

    pedersen(hash_chain(n - 1), n)
}
