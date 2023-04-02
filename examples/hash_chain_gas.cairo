use array::ArrayTrait;
use option::OptionTrait;

// Calculates H(...H(H(0, 1), ..., n))...) where H is the Pedersen hash function.
fn hash_chain(n: felt252) -> felt252 {
    if n == 0 {
        return 0;
    }

    gas::withdraw_gas_all(get_builtin_costs()).expect('Out of gas');
    pedersen(hash_chain(n - 1), n)
}
