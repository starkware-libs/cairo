use traits::Into;
use starknet::ContractAddressIntoFelt252;
use starknet::ContractAddress;

extern type Poseidon;

extern fn hades_permutation(
    s0: felt252, s1: felt252, s2: felt252
) -> (felt252, felt252, felt252) implicits(Poseidon) nopanic;
