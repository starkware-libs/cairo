//! Testing utilities for Starknet contracts.
//!
//! This module provides functions for testing Starknet contracts. The functions
//! allow manipulation of blockchain state and storage variables during tests, as well as
//! inspection of emitted events and messages.
//!
//! Note: The functions in this module can only be used with the `cairo-test` testing framework.
//! If you are using Starknet Foundry, refer to its
//! [documentation](https://foundry-rs.github.io/starknet-foundry/appendix/cheatcodes.html).

#[allow(unused_imports)]
use core::array::{ArrayTrait, SpanTrait};
use core::traits::Into;
use starknet::ContractAddress;

/// A general cheatcode function used to simplify implementation of Starknet testing functions.
///
/// This is the base function used by testing utilities to interact with the test
/// environment. External users can implement custom cheatcodes by injecting a custom
/// `CairoHintProcessor` in the runner.
///
/// # Arguments
///
/// `selector` - The cheatcode identifier.
/// `input` - Input parameters for the cheatcode.
///
/// # Returns
///
/// * A span containing the cheatcode's output
pub extern fn cheatcode<const selector: felt252>(
    input: Span<felt252>,
) -> Span<felt252> implicits() nopanic;

/// Sets the block number to the provided value.
///
/// # Arguments
///
/// `block_number` - The block number to set.
///
/// After a call to `set_block_number`, `starknet::get_execution_info().block_info.block_number`
/// will return the set value.
pub fn set_block_number(block_number: u64) {
    cheatcode::<'set_block_number'>([block_number.into()].span());
}

/// Sets the caller address to the provided value.
///
/// # Arguments
///
/// `address` - The caller address to set.
///
/// After a call to `set_caller_address`, `starknet::get_execution_info().caller_address` will
/// return the set value.
pub fn set_caller_address(address: ContractAddress) {
    cheatcode::<'set_caller_address'>([address.into()].span());
}

/// Sets the contract address to the provided value.
///
/// # Arguments
///
/// `address` - The contract address to set.
///
/// After a call to `set_contract_address`, `starknet::get_execution_info().contract_address` will
/// return the set value.
pub fn set_contract_address(address: ContractAddress) {
    cheatcode::<'set_contract_address'>([address.into()].span());
}

/// Sets the sequencer address to the provided value.
///
/// # Arguments
///
/// `address` - The sequencer address to set.
///
/// After a call to `set_sequencer_address`,
/// `starknet::get_execution_info().block_info.sequencer_address` will return the set value.
pub fn set_sequencer_address(address: ContractAddress) {
    cheatcode::<'set_sequencer_address'>([address.into()].span());
}

/// Sets the block timestamp to the provided value.
///
/// # Arguments
///
/// `block_timestamp` - The block timestamp to set.
///
/// After a call to `set_block_timestamp`,
/// `starknet::get_execution_info().block_info.block_timestamp` will return the set value.
pub fn set_block_timestamp(block_timestamp: u64) {
    cheatcode::<'set_block_timestamp'>([block_timestamp.into()].span());
}

/// Sets the version to the provided value.
///
/// # Arguments
///
/// `version` - The version to set.
///
/// After a call to `set_version`, `starknet::get_execution_info().tx_info.version` will return the
/// set value.
pub fn set_version(version: felt252) {
    cheatcode::<'set_version'>([version].span());
}

/// Sets the account contract address.
///
/// # Arguments
///
/// `address` - The account contract to set.
///
/// After a call to `set_account_contract_address`,
/// `starknet::get_execution_info().tx_info.account_contract_address` will return the set value.
pub fn set_account_contract_address(address: ContractAddress) {
    cheatcode::<'set_account_contract_address'>([address.into()].span());
}

/// Sets the transaction max fee.
///
/// # Arguments
///
/// `fee` - The max fee to set.
///
/// After a call to `set_max_fee`, `starknet::get_execution_info().tx_info.max_fee` will return the
/// set value.
pub fn set_max_fee(fee: u128) {
    cheatcode::<'set_max_fee'>([fee.into()].span());
}

/// Sets the transaction hash.
///
/// # Arguments
///
/// `hash` - The transaction hash to set.
///
/// After a call to `set_transaction_hash`,
/// `starknet::get_execution_info().tx_info.transaction_hash` will return the set value.
pub fn set_transaction_hash(hash: felt252) {
    cheatcode::<'set_transaction_hash'>([hash].span());
}

/// Set the transaction chain id.
///
/// # Arguments
///
/// `chain_id` - The chain id to set.
///
/// After a call to `set_chain_id`, `starknet::get_execution_info().tx_info.chain_id` will return
/// the set value.
pub fn set_chain_id(chain_id: felt252) {
    cheatcode::<'set_chain_id'>([chain_id].span());
}

/// Set the transaction nonce.
///
/// # Arguments
///
/// `non` - The nonce to set.
///
/// After a call to `set_nonce`, `starknet::get_execution_info().tx_info.nonce` will return the set
/// value.
pub fn set_nonce(nonce: felt252) {
    cheatcode::<'set_nonce'>([nonce].span());
}

/// Set the transaction signature.
///
/// # Arguments
///
/// `signature` - The signature to set.
///
/// After a call to `set_signature`, `starknet::get_execution_info().tx_info.signature` will return
/// the set value.
pub fn set_signature(signature: Span<felt252>) {
    cheatcode::<'set_signature'>(signature);
}

/// Set the hash for a block.
///
/// # Arguments
///
/// `block_number` - The targeted block number.
/// `value` - The block hash to set.
///
/// After a call to `set_block_hash`, `starknet::syscalls::get_block_hash_syscall` for the
/// block_number will return the set value.
/// Unset blocks values call would fail.
pub fn set_block_hash(block_number: u64, value: felt252) {
    cheatcode::<'set_block_hash'>([block_number.into(), value].span());
}

/// Pop the earliest unpopped logged event for the contract.
///
/// # Arguments
///
/// `address` - The contract address from which to pop an event.
///
/// The value is returned as a tuple of two spans, the first for the keys and the second for the
/// data.
/// May be called multiple times to pop multiple events.
/// If called until `None` is returned, all events have been popped.
pub fn pop_log_raw(address: ContractAddress) -> Option<(Span<felt252>, Span<felt252>)> {
    let mut log = cheatcode::<'pop_log'>([address.into()].span());
    Some((Serde::deserialize(ref log)?, Serde::deserialize(ref log)?))
}

/// Pop the earliest unpopped logged event for the contract as the requested type.
///
/// # Arguments
///
/// `address` - The contract address from which to pop an event.
///
/// Should be used when the type of the event is known. Type of the event should be the event
/// defined within the contract.
/// Useful for testing the contract's event emission.
/// May be called multiple times to pop multiple events.
/// If called until `None` is returned, all events have been popped.
///
/// # Examples
///
/// ```
/// #[starknet::contract]
/// mod contract {
///    #[event]
///    #[derive(Copy, Drop, Debug, PartialEq, starknet::Event)]
///    pub enum Event {
///       Event1: felt252,
///       Event2: u128,
///    }
///    ...
/// }
///
/// #[test]
/// fn test_event() {
///     let contract_address = somehow_get_contract_address();
///     call_code_causing_events(contract_address);
///     assert_eq!(
///         starknet::testing::pop_log(contract_address), Some(contract::Event::Event1(42))
///     );
///     assert_eq!(
///         starknet::testing::pop_log(contract_address), Some(contract::Event::Event2(41))
///     );
///     assert_eq!(
///         starknet::testing::pop_log(contract_address), Some(contract::Event::Event1(40))
///     );
///     assert_eq!(starknet::testing::pop_log_raw(contract_address), None);
/// }
/// ```
pub fn pop_log<T, +starknet::Event<T>>(address: ContractAddress) -> Option<T> {
    let (mut keys, mut data) = pop_log_raw(address)?;
    starknet::Event::deserialize(ref keys, ref data)
}

// TODO(Ilya): Decide if we limit the type of `to_address`.
/// Pop the earliest unpopped l2 to l1 message for the contract.
///
/// # Arguments
///
/// `address` - The contract address from which to pop a l2-L1 message.
///
/// The returned value is a tuple of the l1 address the message was sent to as a `felt252`, and the
/// message data as a span.
/// May be called multiple times to pop multiple messages.
/// Useful for testing the contract's l2 to l1 message emission.
pub fn pop_l2_to_l1_message(address: ContractAddress) -> Option<(felt252, Span<felt252>)> {
    let mut l2_to_l1_message = cheatcode::<'pop_l2_to_l1_message'>([address.into()].span());
    Some((Serde::deserialize(ref l2_to_l1_message)?, Serde::deserialize(ref l2_to_l1_message)?))
}
