use starknet::{
    SyscallResultTrait, SyscallResult, syscalls::get_execution_info_syscall,
    contract_address::ContractAddress
};
use box::BoxTrait;

#[derive(Copy, Drop)]
struct ExecutionInfo {
    block_info: Box<BlockInfo>,
    tx_info: Box<TxInfo>,
    caller_address: ContractAddress,
    contract_address: ContractAddress,
    entry_point_selector: felt252,
}

#[derive(Copy, Drop, Serde)]
struct BlockInfo {
    block_number: u64,
    block_timestamp: u64,
    sequencer_address: ContractAddress,
}

#[derive(Copy, Drop, Serde)]
struct TxInfo {
    // The version of the transaction. It is fixed (currently, 1) in the OS, and should be
    // signed by the account contract.
    // This field allows invalidating old transactions, whenever the meaning of the other
    // transaction fields is changed (in the OS).
    version: felt252,
    // The account contract from which this transaction originates.
    account_contract_address: ContractAddress,
    // The max_fee field of the transaction.
    max_fee: u128,
    // The signature of the transaction.
    signature: Span<felt252>,
    // The hash of the transaction.
    transaction_hash: felt252,
    // The identifier of the chain.
    // This field can be used to prevent replay of testnet transactions on mainnet.
    chain_id: felt252,
    // The transaction's nonce.
    nonce: felt252,
}

fn get_execution_info() -> Box<ExecutionInfo> {
    get_execution_info_syscall().unwrap_syscall()
}

fn get_caller_address() -> ContractAddress {
    get_execution_info().unbox().caller_address
}

fn get_contract_address() -> ContractAddress {
    get_execution_info().unbox().contract_address
}

fn get_block_info() -> Box<BlockInfo> {
    get_execution_info().unbox().block_info
}

fn get_tx_info() -> Box<TxInfo> {
    get_execution_info().unbox().tx_info
}

fn get_block_timestamp() -> u64 {
    get_block_info().unbox().block_timestamp
}

fn get_block_number() -> u64 {
    get_block_info().unbox().block_number
}

/// The extended version of the `get_execution_info` syscall result.
mod v2 {
    use starknet::contract_address::ContractAddress;
    use super::BlockInfo;

    #[derive(Copy, Drop)]
    struct ExecutionInfo {
        block_info: Box<BlockInfo>,
        tx_info: Box<TxInfo>,
        caller_address: ContractAddress,
        contract_address: ContractAddress,
        entry_point_selector: felt252,
    }

    #[derive(Copy, Drop, Serde)]
    struct TxInfo {
        // The version of the transaction. It is fixed (currently, 1) in the OS, and should be
        // signed by the account contract.
        // This field allows invalidating old transactions, whenever the meaning of the other
        // transaction fields is changed (in the OS).
        version: felt252,
        // The account contract from which this transaction originates.
        account_contract_address: ContractAddress,
        // The max_fee field of the transaction.
        max_fee: u128,
        // The signature of the transaction.
        signature: Span<felt252>,
        // The hash of the transaction.
        transaction_hash: felt252,
        // The identifier of the chain.
        // This field can be used to prevent replay of testnet transactions on mainnet.
        chain_id: felt252,
        // The transaction's nonce.
        nonce: felt252,
        // A span of ResourceBounds structs.
        resource_bounds: Span<ResourceBounds>,
        // The tip.
        tip: u128,
        // If specified, the paymaster should pay for the execution of the tx.
        // The data includes the address of the paymaster sponsoring the transaction, followed by
        // extra data to send to the paymaster.
        paymaster_data: Span<felt252>,
        // The data availability mode for the nonce.
        nonce_data_availabilty_mode: u32,
        // The data availability mode for the account balance from which fee will be taken.
        fee_data_availabilty_mode: u32,
        // If nonempty, will contain the required data for deploying and initializing an account
        // contract: its class hash, address salt and constructor calldata.
        account_deployment_data: Span<felt252>,
    }

    #[derive(Copy, Drop, Serde)]
    struct ResourceBounds {
        // The name of the resource.
        resource: felt252,
        // The maximum amount of the resource allowed for usage during the execution.
        max_amount: u64,
        // The maximum price the user is willing to pay for the resource unit.
        max_price_per_unit: u128,
    }
}
