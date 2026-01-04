// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0
// (token/src/erc721/extensions/erc721_enumerable/interface.cairo)

use starknet::ContractAddress;

pub const IERC721ENUMERABLE_ID: felt252 =
    0x16bc0f502eeaf65ce0b3acb5eea656e2f26979ce6750e8502a82f377e538c87;

#[starknet::interface]
pub trait IERC721Enumerable<TState> {
    fn total_supply(self: @TState) -> u256;
    fn token_by_index(self: @TState, index: u256) -> u256;
    fn token_of_owner_by_index(self: @TState, owner: ContractAddress, index: u256) -> u256;
}

#[starknet::interface]
pub trait ERC721EnumerableABI<TState> {
    fn total_supply(self: @TState) -> u256;
    fn token_by_index(self: @TState, index: u256) -> u256;
    fn token_of_owner_by_index(self: @TState, owner: ContractAddress, index: u256) -> u256;
    fn all_tokens_of_owner(self: @TState, owner: ContractAddress) -> Span<u256>;
}
