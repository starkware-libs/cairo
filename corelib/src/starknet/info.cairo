//! Information about the Starknet execution environment.
//!
//! This module provides access to runtime information about the current transaction,
//! block, and execution context in a Starknet smart contract. It enables contracts
//! to access execution context data.
//!
//! # Examples
//!
//! ```
//! use starknet::{get_block_info, get_caller_address, get_contract_address};
//!
//! // Get block information
//! let block_info = get_block_info().unbox();
//! let timestamp = block_info.block_timestamp;
//!
//! // Get caller and contract addresses
//! let caller = get_caller_address();
//! let contract = get_contract_address();
//! ```

#[allow(unused_imports)]
use core::box::BoxTrait;
#[allow(unused_imports)]
use starknet::contract_address::ContractAddress;
#[allow(unused_imports)]
use starknet::syscalls::get_execution_info_syscall;
#[allow(unused_imports)]
use starknet::{SyscallResult, SyscallResultTrait};

/// The execution information for the current entry point.
#[derive(Copy, Drop, Debug)]
pub struct ExecutionInfo {
    /// Information about the current block.
    pub block_info: Box<BlockInfo>,
    /// Information about the current transaction.
    pub tx_info: Box<TxInfo>,
    /// The address of the caller contract.
    /// Returns 0 if the current execution was externally triggered, this happens when the
    /// transaction begins its execution in an account contract.
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

/// Information about the current transaction.
#[derive(Copy, Drop, Debug, Serde)]
pub struct TxInfo {
    /// The version of the transaction.
    /// This field allows invalidating old transactions, whenever the meaning of the other
    /// transaction fields is changed (in the OS).
    pub version: felt252,
    /// The account contract from which this transaction originates.
    pub account_contract_address: ContractAddress,
    /// The `max_fee` field of the transaction.
    pub max_fee: u128,
    /// The signature of the transaction.
    pub signature: Span<felt252>,
    /// The hash of the transaction.
    pub transaction_hash: felt252,
    /// The identifier of the chain.
    /// This field can be used to prevent replay of testnet transactions on mainnet.
    pub chain_id: felt252,
    /// The transaction's nonce.
    pub nonce: felt252,
}

/// Returns the execution info for the current execution.
///
/// # Examples
///
/// ```
/// use starknet::get_execution_info;
///
/// let execution_info = get_execution_info().unbox();
///
/// // Access various execution context information
/// let caller = execution_info.caller_address;
/// let contract = execution_info.contract_address;
/// let selector = execution_info.entry_point_selector;
/// ```
pub fn get_execution_info() -> Box<v2::ExecutionInfo> {
    starknet::syscalls::get_execution_info_v2_syscall().unwrap_syscall()
}

/// Returns the address of the caller contract.
///
/// # Examples
///
/// ```
/// use starknet::get_caller_address;
///
/// let caller = get_caller_address();
/// ```
pub fn get_caller_address() -> ContractAddress {
    get_execution_info().caller_address
}

/// Returns the address of the contract being executed.
///
/// # Examples
///
/// ```
/// use starknet::get_contract_address;
///
/// let contract_address = get_contract_address();
/// ```
pub fn get_contract_address() -> ContractAddress {
    get_execution_info().contract_address
}

/// Returns the block information for the current block.
///
/// # Examples
///
/// ```
/// use starknet::get_block_info;
///
/// let block_info = get_block_info().unbox();
///
/// let block_number = block_info.block_number;
/// let block_timestamp = block_info.block_timestamp;
/// let sequencer = block_info.sequencer_address;
/// ```
pub fn get_block_info() -> Box<BlockInfo> {
    get_execution_info().block_info
}

/// Returns the transaction information for the current transaction.
///
/// # Examples
///
/// ```
/// use starknet::get_tx_info;
///
/// let tx_info = get_tx_info().unbox();
///
/// let account_contract_address = tx_info.account_contract_address;
/// let chain_id = tx_info.chain_id;
/// let nonce = tx_info.nonce;
/// let max_fee = tx_info.max_fee;
/// let tx_hash = tx_info.transaction_hash;
/// let signature = tx_info.signature;
/// let version = tx_info.version;
/// ```
pub fn get_tx_info() -> Box<v2::TxInfo> {
    get_execution_info().tx_info
}

/// Returns the timestamp of the current block.
///
/// # Examples
///
/// ```
/// use starknet::get_block_timestamp;
///
/// let block_timestamp = get_block_timestamp();
/// ```
pub fn get_block_timestamp() -> u64 {
    get_block_info().block_timestamp
}

/// Returns the number of the current block.
///
/// # Examples
///
/// ```
/// use starknet::get_block_number;
///
/// let block_number = get_block_number();
/// ```
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

    /// Extended information about the current transaction.
    #[derive(Copy, Drop, Debug, Serde)]
    pub struct TxInfo {
        /// The version of the transaction. It is fixed (currently, 1) in the OS, and should be
        /// signed by the account contract.
        /// This field allows invalidating old transactions, whenever the meaning of the other
        /// transaction fields is changed (in the OS).
        pub version: felt252,
        /// The account contract from which this transaction originates.
        pub account_contract_address: ContractAddress,
        /// The `max_fee` field of the transaction.
        pub max_fee: u128,
        /// The signature of the transaction.
        pub signature: Span<felt252>,
        /// The hash of the transaction.
        pub transaction_hash: felt252,
        /// The identifier of the chain.
        /// This field can be used to prevent replay of testnet transactions on mainnet.
        pub chain_id: felt252,
        /// The transaction's nonce.
        pub nonce: felt252,
        /// A span of `ResourceBounds` structs used for V3 transactions.
        pub resource_bounds: Span<ResourceBounds>,
        /// The tip of the transaction.
        pub tip: u128,
        /// If specified, the paymaster should pay for the execution of the transaction.
        /// The data includes the address of the paymaster sponsoring the transaction, followed by
        /// extra data to send to the paymaster.
        /// Used for V3 transactions.
        pub paymaster_data: Span<felt252>,
        /// The data availability mode for the nonce.
        /// Used for V3 transactions.
        pub nonce_data_availability_mode: u32,
        /// The data availability mode for the account balance from which fee will be taken.
        /// Used for V3 transactions.
        pub fee_data_availability_mode: u32,
        /// If nonempty, will contain the required data for deploying and initializing an account
        /// contract: its class hash, address salt and constructor calldata.
        /// Used for V3 transactions.
        pub account_deployment_data: Span<felt252>,
    }

    /// V3 transactions resources used for enabling the fee market.
    #[derive(Copy, Drop, Debug, Serde)]
    pub struct ResourceBounds {
        /// The name of the resource.
        pub resource: felt252,
        /// The maximum amount of the resource allowed for usage during the execution.
        pub max_amount: u64,
        /// The maximum price the user is willing to pay for the resource unit.
        pub max_price_per_unit: u128,
    }
}
