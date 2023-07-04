use starknet::{
    SyscallResultTrait, SyscallResult, syscalls::get_execution_info_syscall,
    contract_address::ContractAddress
};
use box::BoxTrait;

#[derive(Copy, Drop)]
pub struct ExecutionInfo {
    block_info: Box<BlockInfo>,
    tx_info: Box<TxInfo>,
    caller_address: ContractAddress,
    contract_address: ContractAddress,
    entry_point_selector: felt252,
}

#[derive(Copy, Drop)]
pub struct BlockInfo {
    pub block_number: u64,
    pub block_timestamp: u64,
    pub sequencer_address: ContractAddress,
}

#[derive(Copy, Drop)]
pub struct TxInfo {
    // The version of the transaction. It is fixed (currently, 1) in the OS, and should be
    // signed by the account contract.
    // This field allows invalidating old transactions, whenever the meaning of the other
    // transaction fields is changed (in the OS).
    pub version: felt252,
    // The account contract from which this transaction originates.
    pub account_contract_address: ContractAddress,
    // The max_fee field of the transaction.
    pub max_fee: u128,
    // The signature of the transaction.
    pub signature: Span<felt252>,
    // The hash of the transaction.
    pub transaction_hash: felt252,
    // The identifier of the chain.
    // This field can be used to prevent replay of testnet transactions on mainnet.
    pub chain_id: felt252,
    // The transaction's nonce.
    pub nonce: felt252,
}

pub fn get_execution_info() -> Box<ExecutionInfo> {
    get_execution_info_syscall().unwrap_syscall()
}

pub fn get_caller_address() -> ContractAddress {
    get_execution_info().unbox().caller_address
}

pub fn get_contract_address() -> ContractAddress {
    get_execution_info().unbox().contract_address
}

pub fn get_block_info() -> Box<BlockInfo> {
    get_execution_info().unbox().block_info
}

pub fn get_tx_info() -> Box<TxInfo> {
    get_execution_info().unbox().tx_info
}

pub fn get_block_timestamp() -> u64 {
    get_block_info().unbox().block_timestamp
}

pub fn get_block_number() -> u64 {
    get_block_info().unbox().block_number
}
