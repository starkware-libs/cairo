use traits::Into;
use starknet::ContractAddressIntoFelt252;
use starknet::ContractAddress;

extern type Poseidon;

extern fn poseidon(
    s0: felt252, s1: felt252, s2: felt252
) -> (felt252, felt252, felt252) implicits(Poseidon) nopanic;


// Hashes two elements and retrieves a single field element output.
fn poseidon_hash(x: felt252, y: felt252) -> felt252 {
    // To distinguish between the use cases the capacity element is initialized to 2.
    let (r0, r1, r2) = poseidon(x, y, 2);
    r0
}
