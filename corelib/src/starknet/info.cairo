use starknet::{
    SyscallResultTrait, SyscallResult, syscalls::get_execution_info_syscall,
    contract_address::ContractAddress
};
use core::box::BoxTrait;


/// The execution for the current entry point.
#[derive(Copy, Drop, Debug)]
pub struct ExecutionInfo {
    /// Information about the current block.
    pub block_info: Box<BlockInfo>,
    /// Information about the current transaction.
    pub tx_info: Box<TxInfo>,
    /// The address of the caller contract.
    /// Returns 0 if there current execution was externally triggered, this happens when the
    /// transaction begins it execution in an account contract.
    pub caller_address: ContractAddress,
    /// The address of the contract being executed.
    /// Storage writes and emitted events are associated with this address.
    pub contract_address: ContractAddress,
    /// The selector of the current entry point.
    pub entry_point_selector: felt252,
}

/// Information about the current block.
#[derive(Copy, Drop, Debug, Serde)]
pub struct BlockInfo {
    /// The number, that is, the height, of this block.
    pub block_number: u64,
    /// The time at which the sequencer began building the block, in seconds since the Unix epoch.
    pub block_timestamp: u64,
    /// The Starknet address of the sequencer that created the block.
    pub sequencer_address: ContractAddress,
}

#[derive(Copy, Drop, Debug, Serde)]
pub struct TxInfo {
    // The version of the transaction.
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

/// Returns the execution info for the current execution.
pub fn get_execution_info() -> Box<v2::ExecutionInfo> {
    starknet::syscalls::get_execution_info_v2_syscall().unwrap_syscall()
}


/// Returns the address of the caller contract.
///
/// See `ExecutionInfo.caller_address` for more information.
pub fn get_caller_address() -> ContractAddress {
    get_execution_info().caller_address
}

/// Returns the address of the contract being executed.
///
/// See `ExecutionInfo.contract_address` for more information.
pub fn get_contract_address() -> ContractAddress {
    get_execution_info().contract_address
}

/// Returns the block info for the current block.
pub fn get_block_info() -> Box<BlockInfo> {
    get_execution_info().block_info
}

/// Returns the transaction info for the current transaction.
pub fn get_tx_info() -> Box<v2::TxInfo> {
    get_execution_info().tx_info
}

/// Returns the timestamp of the current block.
pub fn get_block_timestamp() -> u64 {
    get_block_info().block_timestamp
}

/// Returns the number of the current block.
pub fn get_block_number() -> u64 {
    get_block_info().block_number
}

/// The extended version of the `get_execution_info` syscall result.
pub mod v2 {
    use starknet::contract_address::ContractAddress;
    use super::BlockInfo;


    /// The same as `ExecutionInfo`, but with the `TxInfo` field replaced with `v2::TxInfo`.
    #[derive(Copy, Drop, Debug)]
    pub struct ExecutionInfo {
        pub block_info: Box<BlockInfo>,
        pub tx_info: Box<TxInfo>,
        pub caller_address: ContractAddress,
        pub contract_address: ContractAddress,
        pub entry_point_selector: felt252,
    }

    #[derive(Copy, Drop, Debug, Serde)]
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
        // A span of ResourceBounds structs.
        pub resource_bounds: Span<ResourceBounds>,
        // The tip.
        pub tip: u128,
        // If specified, the paymaster should pay for the execution of the tx.
        // The data includes the address of the paymaster sponsoring the transaction, followed by
        // extra data to send to the paymaster.
        pub paymaster_data: Span<felt252>,
        // The data availability mode for the nonce.
        pub nonce_data_availability_mode: u32,
        // The data availability mode for the account balance from which fee will be taken.
        pub fee_data_availability_mode: u32,
        // If nonempty, will contain the required data for deploying and initializing an account
        // contract: its class hash, address salt and constructor calldata.
        pub account_deployment_data: Span<felt252>,
    }

    #[derive(Copy, Drop, Debug, Serde)]
    pub struct ResourceBounds {
        // The name of the resource.
        pub resource: felt252,
        // The maximum amount of the resource allowed for usage during the execution.
        pub max_amount: u64,
        // The maximum price the user is willing to pay for the resource unit.
        pub max_price_per_unit: u128,
    }
}
