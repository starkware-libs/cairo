// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (access/src/ownable/interface.cairo)

use starknet::ContractAddress;

#[starknet::interface]
pub trait IOwnable<TState> {
    fn owner(self: @TState) -> ContractAddress;
    fn transfer_ownership(ref self: TState, new_owner: ContractAddress);
    fn renounce_ownership(ref self: TState);
}

#[starknet::interface]
pub trait IOwnableCamelOnly<TState> {
    fn transferOwnership(ref self: TState, newOwner: ContractAddress);
    fn renounceOwnership(ref self: TState);
}

#[starknet::interface]
pub trait OwnableABI<TState> {
    // IOwnable
    fn owner(self: @TState) -> ContractAddress;
    fn transfer_ownership(ref self: TState, new_owner: ContractAddress);
    fn renounce_ownership(ref self: TState);

    // IOwnableCamelOnly
    fn transferOwnership(ref self: TState, newOwner: ContractAddress);
    fn renounceOwnership(ref self: TState);
}

#[starknet::interface]
pub trait IOwnableTwoStep<TState> {
    fn owner(self: @TState) -> ContractAddress;
    fn pending_owner(self: @TState) -> ContractAddress;
    fn accept_ownership(ref self: TState);
    fn transfer_ownership(ref self: TState, new_owner: ContractAddress);
    fn renounce_ownership(ref self: TState);
}

#[starknet::interface]
pub trait IOwnableTwoStepCamelOnly<TState> {
    fn pendingOwner(self: @TState) -> ContractAddress;
    fn acceptOwnership(ref self: TState);
    fn transferOwnership(ref self: TState, newOwner: ContractAddress);
    fn renounceOwnership(ref self: TState);
}

#[starknet::interface]
pub trait OwnableTwoStepABI<TState> {
    // IOwnableTwoStep
    fn owner(self: @TState) -> ContractAddress;
    fn pending_owner(self: @TState) -> ContractAddress;
    fn accept_ownership(ref self: TState);
    fn transfer_ownership(ref self: TState, new_owner: ContractAddress);
    fn renounce_ownership(ref self: TState);

    // IOwnableTwoStepCamelOnly
    fn pendingOwner(self: @TState) -> ContractAddress;
    fn acceptOwnership(ref self: TState);
    fn transferOwnership(ref self: TState, newOwner: ContractAddress);
    fn renounceOwnership(ref self: TState);
}
