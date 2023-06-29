use starknet::ContractAddress;
use array::ArrayTrait;
use array::SpanTrait;
use traits::Into;

extern fn pop_log(
    address: ContractAddress
) -> Option<(Span<felt252>, Span<felt252>)> implicits() nopanic;

// A general cheatcode function used to simplify implementation of Starknet testing functions.
// External users of the cairo crates can also implement their own cheatcodes
// by injecting custom `CairoHintProcessor`.
extern fn cheatcode<const selector: felt252>(
    input: Span<felt252>
) -> Span<felt252> implicits() nopanic;

// Set the block number to the provided value.
fn set_block_number(block_number: u64) {
    cheatcode::<'set_block_number'>(array![block_number.into()].span());
}

// Set the caller address to the provided value.
fn set_caller_address(address: ContractAddress) {
    cheatcode::<'set_caller_address'>(array![address.into()].span());
}

// Set the contract address to the provided value.
fn set_contract_address(address: ContractAddress) {
    cheatcode::<'set_contract_address'>(array![address.into()].span());
}

// Set the sequencer address to the provided value.
fn set_sequencer_address(address: ContractAddress) {
    cheatcode::<'set_sequencer_address'>(array![address.into()].span());
}

// Set the block timestamp to the provided value.
fn set_block_timestamp(block_timestamp: u64) {
    cheatcode::<'set_block_timestamp'>(array![block_timestamp.into()].span());
}

// Set the version to the provided value.
fn set_version(version: felt252) {
    cheatcode::<'set_version'>(array![version].span());
}

// Set the account contract address.
fn set_account_contract_address(address: ContractAddress) {
    cheatcode::<'set_account_contract_address'>(array![address.into()].span());
}

// Set the max fee.
fn set_max_fee(fee: u128) {
    cheatcode::<'set_max_fee'>(array![fee.into()].span());
}

// Set the transaction hash.
fn set_transaction_hash(hash: felt252) {
    cheatcode::<'set_transaction_hash'>(array![hash].span());
}

// Set the chain id.
fn set_chain_id(chain_id: felt252) {
    cheatcode::<'set_chain_id'>(array![chain_id].span());
}

// Set the nonce.
fn set_nonce(nonce: felt252) {
    cheatcode::<'set_nonce'>(array![nonce].span());
}

// Set the signature.
fn set_signature(signature: Span<felt252>) {
    cheatcode::<'set_signature'>(signature);
}
