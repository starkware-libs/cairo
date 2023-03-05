use starknet::ContractAddress;

extern fn set_caller_address(address: ContractAddress) implicits() nopanic;
extern fn set_contract_address(address: ContractAddress) implicits() nopanic;
extern fn set_sequencer_address(address: ContractAddress) implicits() nopanic;
extern fn set_block_number(block_number: u64) implicits() nopanic;
extern fn set_block_timestamp(block_timestamp: u64) implicits() nopanic;
