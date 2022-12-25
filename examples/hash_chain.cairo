// Calculates H(...H(H(0, 1), ..., n))...) where H is the Pedersen hash function.
fn hash_chain(n: felt) -> felt {
    if n == 0 {
        return 0;
    }

    pedersen(hash_chain(n - 1), n)
}
