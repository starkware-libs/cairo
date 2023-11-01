// Calculates H(...H(H(0, 1), ..., n))...) where H is the Pedersen hash function.
fn hash_chain(n: felt252) -> felt252 {
    if n == 0 {
        return 0;
    }

    core::gas::withdraw_gas_all(core::gas::get_builtin_costs()).expect('Out of gas');
    core::pedersen::pedersen(hash_chain(n - 1), n)
}
