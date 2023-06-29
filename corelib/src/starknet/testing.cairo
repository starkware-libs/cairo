use starknet::ContractAddress;
use array::ArrayTrait;
use array::SpanTrait;
use traits::Into;

extern fn set_caller_address(address: ContractAddress) nopanic;
extern fn set_contract_address(address: ContractAddress) nopanic;
extern fn set_sequencer_address(address: ContractAddress) nopanic;
extern fn set_block_timestamp(block_timestamp: u64) nopanic;
extern fn set_version(version: felt252) nopanic;
extern fn set_account_contract_address(address: ContractAddress) nopanic;
extern fn set_max_fee(fee: u128) nopanic;
extern fn set_transaction_hash(hash: felt252) nopanic;
extern fn set_chain_id(chain_id: felt252) nopanic;
extern fn set_nonce(nonce: felt252) nopanic;
extern fn set_signature(signature: Span<felt252>) nopanic;
extern fn pop_log(address: ContractAddress) -> Option<(Span<felt252>, Span<felt252>)> nopanic;

// A general cheatcode function used to simplify implementation of Starknet testing functions.
// External users of the cairo crates can also implement their own cheatcodes
// by injecting custom `CairoHintProcessor`.
extern fn cheatcode<const selector: felt252>(input: Span<felt252>) -> Span<felt252> nopanic;

// Set the block number to the provided value.
fn set_block_number(block_number: u64) {
    let mut data = ArrayTrait::new();
    data.append(block_number.into());

    cheatcode::<'set_block_number'>(data.span());
}
