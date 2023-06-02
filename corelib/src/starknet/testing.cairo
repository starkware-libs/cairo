use starknet::ContractAddress;

#[derive(Drop)]
struct Log {
    keys: Array<felt252>,
    data: Array<felt252>,
}

extern fn set_caller_address(address: ContractAddress) implicits() nopanic;
extern fn set_contract_address(address: ContractAddress) implicits() nopanic;
extern fn set_sequencer_address(address: ContractAddress) implicits() nopanic;
extern fn set_block_number(block_number: u64) implicits() nopanic;
extern fn set_block_timestamp(block_timestamp: u64) implicits() nopanic;
extern fn set_version(version: felt252) implicits() nopanic;
extern fn set_account_contract_address(address: ContractAddress) implicits() nopanic;
extern fn set_max_fee(fee: u128) implicits() nopanic;
extern fn set_transaction_hash(hash: felt252) implicits() nopanic;
extern fn set_chain_id(chain_id: felt252) implicits() nopanic;
extern fn set_nonce(nonce: felt252) implicits() nopanic;
extern fn set_signature(signature: Span<felt252>) implicits() nopanic;
extern fn pop_logs(address: ContractAddress) -> Array<Log> implicits() nopanic;
