// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (utils/src/cryptography/interface.cairo)

use starknet::ContractAddress;

#[starknet::interface]
pub trait INonces<TState> {
    fn nonces(self: @TState, owner: ContractAddress) -> felt252;
}

#[starknet::interface]
pub trait ISNIP12Metadata<TState> {
    fn snip12_metadata(self: @TState) -> (felt252, felt252);
}
