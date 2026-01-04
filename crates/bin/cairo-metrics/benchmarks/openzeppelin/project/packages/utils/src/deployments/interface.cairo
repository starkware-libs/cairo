// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (utils/src/deployments/interface.cairo)

use starknet::{ClassHash, ContractAddress};

#[starknet::interface]
pub trait IUniversalDeployer<TState> {
    fn deploy_contract(
        ref self: TState,
        class_hash: ClassHash,
        salt: felt252,
        not_from_zero: bool,
        calldata: Span<felt252>,
    ) -> ContractAddress;
}

#[starknet::interface]
pub trait IUniversalDeployerCamel<TState> {
    fn deployContract(
        ref self: TState,
        classHash: ClassHash,
        salt: felt252,
        notFromZero: bool,
        calldata: Span<felt252>,
    ) -> ContractAddress;
}

#[starknet::interface]
pub trait UniversalDeployerABI<TState> {
    fn deploy_contract(
        ref self: TState,
        class_hash: ClassHash,
        salt: felt252,
        not_from_zero: bool,
        calldata: Span<felt252>,
    ) -> ContractAddress;

    fn deployContract(
        ref self: TState,
        classHash: ClassHash,
        salt: felt252,
        notFromZero: bool,
        calldata: Span<felt252>,
    ) -> ContractAddress;
}
