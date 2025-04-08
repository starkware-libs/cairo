//! Account module defining the [`Call`] struct and the [`AccountContract`] trait.
//!
//! The `Call` struct represents a call to a contract, with the following fields:
//! - `to`: The address of the contract to call.
//! - `selector`: The entry point selector in the called contract.
//! - `calldata`: The calldata to pass to the entry point.
//!
//! The `AccountContract` trait defines the standard interface for account contracts. It assumes
//! that the calldata for invoke transactions is an `Array<Call>`, following the SNIP6 standard.
//!
//! Implementing this trait allows contracts to function as account contracts in the Starknet
//! network, supporting class declarations and batched call execution.

use starknet::ContractAddress;

/// A struct representing a call to a contract.
#[derive(Drop, Copy, Serde, Debug)]
pub struct Call {
    /// The address of the contract to call.
    pub to: ContractAddress,
    /// The entry point selector in the called contract.
    pub selector: felt252,
    /// The calldata to pass to entry point.
    pub calldata: Span<felt252>,
}

/// A trait for account contracts that support class declarations (only `__validate__` and
/// `__execute__` are mandatory for an account).
///
/// This trait assumes that the calldata for invoke transactions is `Array<Call>`.
/// This is the network standard following SNIP6.
/// It is not enforced by Starknet, but deviating from the standard interface may lead to
/// incompatibility with standard tooling.
#[starknet::interface]
pub trait AccountContract<TContractState> {
    /// An entry point that is called to check if the account is willing to pay for the declaration
    /// of the class with the given hash.
    /// The entry point should return `starknet::VALIDATED` if the account is willing to pay
    /// for the declaration.
    fn __validate_declare__(self: @TContractState, class_hash: felt252) -> felt252;

    /// An entry point that is called to check if the account is willing to pay for
    /// executing a given set of calls.
    /// The entry point should return `starknet::VALIDATED` if the account is willing to pay
    /// for the execution, in which case `__execute__` will be called on the same set of calls.
    fn __validate__(ref self: TContractState, calls: Array<Call>) -> felt252;

    /// An entry point that is called to execute a given set of calls.
    /// This entry point should block the deprecated v0 invoke transactions as they do not go
    /// through the `__validate__` entry point.
    fn __execute__(ref self: TContractState, calls: Array<Call>) -> Array<Span<felt252>>;
}
