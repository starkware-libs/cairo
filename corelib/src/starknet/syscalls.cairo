//! Utilities for interacting with the Starknet OS.
//!
//! Writing smart contracts requires various associated operations, such as calling another contract
//! or accessing the contract’s storage, that standalone programs do not require. Cairo supports
//! these operations by using system calls.
//!
//! System calls enable a contract to require services from the Starknet OS. You can use system
//! calls in a function to get information that depends on the broader state of Starknet, such as
//! the current timestamp of the address of the caller, but also to modify the state of Starknet by,
//! for example, storing values in a contract's storage or deploying new contracts.

use core::gas::GasBuiltin;
use starknet::{
    SyscallResult, storage_access::StorageAddress, class_hash::ClassHash,
    contract_address::ContractAddress,
};

/// Calls a given contract.
///
/// # Arguments
///
/// * `address` - The address of the called contract.
/// * `entry_point_selector` - A selector for a function within that contract.
/// * `calldata` - Call arguments.
pub extern fn call_contract_syscall(
    address: ContractAddress, entry_point_selector: felt252, calldata: Span<felt252>,
) -> SyscallResult<Span<felt252>> implicits(GasBuiltin, System) nopanic;

/// Deploys a new instance of a previously declared class.
///
/// # Arguments
///
/// * `class_hash` - The class hash of the contract to be deployed.
/// * `contract_address_salt` - The salt, an arbitrary value provided by the deployer, used in the
///  computation of the contract's address.
/// * `calldata` - Call arguments for the constructor.
/// * `deploy_from_zero` - Deploy the contract from the zero address.
///
/// # Returns
///
/// * The address of the deployed contract.
/// * The serialized return value of the constructor.
pub extern fn deploy_syscall(
    class_hash: ClassHash,
    contract_address_salt: felt252,
    calldata: Span<felt252>,
    deploy_from_zero: bool,
) -> SyscallResult<(ContractAddress, Span<felt252>)> implicits(GasBuiltin, System) nopanic;

/// Emits an event.
///
/// # Arguments
///
/// * `keys` - The keys of the event.
/// * `data` - The data of the event.
pub extern fn emit_event_syscall(
    keys: Span<felt252>, data: Span<felt252>,
) -> SyscallResult<()> implicits(GasBuiltin, System) nopanic;

/// Returns the hash of the block with the given number.
///
/// # Arguments
///
/// * `block_number` - The number of the queried block.
pub extern fn get_block_hash_syscall(
    block_number: u64,
) -> SyscallResult<felt252> implicits(GasBuiltin, System) nopanic;

/// Gets information about the currently executing block and the transactions in the block. For a
/// complete description of this information, see [`Execution information`].
///
/// Returns a box containing the current execution information.
pub extern fn get_execution_info_syscall() -> SyscallResult<
    Box<starknet::info::ExecutionInfo>,
> implicits(GasBuiltin, System) nopanic;

/// Gets information about the current execution, version 2.
///
/// Returns a box containing the current V2 execution information.
pub extern fn get_execution_info_v2_syscall() -> SyscallResult<
    Box<starknet::info::v2::ExecutionInfo>,
> implicits(GasBuiltin, System) nopanic;

/// Calls the requested function in any previously declared class.
///
/// # Arguments
///
/// * `class_hash` - The hash of the class to be used.
/// * `function_selector` - A selector for a function within that class.
/// * `calldata` - Call arguments.
pub extern fn library_call_syscall(
    class_hash: ClassHash, function_selector: felt252, calldata: Span<felt252>,
) -> SyscallResult<Span<felt252>> implicits(GasBuiltin, System) nopanic;

// TODO(Ilya): Decide if we limit the type of `to_address`.
/// Sends a message to L1.
///
/// # Arguments
///
/// * `to_address` - The recipient's L1 address.
/// * `payload` - The content of the message.
pub extern fn send_message_to_l1_syscall(
    to_address: felt252, payload: Span<felt252>,
) -> SyscallResult<()> implicits(GasBuiltin, System) nopanic;

/// Gets the value of a key in the storage of the calling contract.
///
/// # Arguments
///
/// * `address_domain` - The domain of the address. Only `address_domain` 0 is currently supported,
/// in the future it will enable access to address spaces with different data availability
/// guarantees.
/// * `address` - The address of the storage key to read.
pub extern fn storage_read_syscall(
    address_domain: u32, address: StorageAddress,
) -> SyscallResult<felt252> implicits(GasBuiltin, System) nopanic;

/// Sets the value of a key in the storage of the calling contract.
///
/// # Arguments
///
/// * `address_domain` - The domain of the address. Only `address_domain` 0 is currently supported,
/// in the future it will enable access to address spaces with different data availability
/// guarantees.
/// * `address` - The address of the storage key to write.
/// * `value` - The value to write to the key.
pub extern fn storage_write_syscall(
    address_domain: u32, address: StorageAddress, value: felt252,
) -> SyscallResult<()> implicits(GasBuiltin, System) nopanic;

/// Replaces the class hash of the current contract.
///
/// # Arguments
///
/// * `class_hash` - The class hash that should replace the current one.
pub extern fn replace_class_syscall(
    class_hash: ClassHash,
) -> SyscallResult<()> implicits(GasBuiltin, System) nopanic;

/// Gets the class hash of the contract at the given address.
///
/// # Arguments
///
/// * `contract_address` - The address of the deployed contract.
///
/// Returns the class hash of the contract's originating code.
pub extern fn get_class_hash_at_syscall(
    contract_address: ContractAddress,
) -> SyscallResult<ClassHash> implicits(GasBuiltin, System) nopanic;

/// Computes the keccak of the input.
///
/// # Arguments
///
/// * `input` - The input provided to the keccak function.
///
/// The system call does not add any padding and the input needs to be a multiple of 1088 bits
/// (== 17 u64 word).
pub extern fn keccak_syscall(
    input: Span<u64>,
) -> SyscallResult<u256> implicits(GasBuiltin, System) nopanic;

/// Computes the next SHA-256 state of the input with the given state.
///
/// # Arguments
///
/// * `state` - The current SHA-256 state.
/// * `input` - The input provided to compute the next SHA-256 state.
///
/// The system call does not add any padding and the input needs to be a multiple of 512 bits
/// (== 16 u32 word).
pub extern fn sha256_process_block_syscall(
    state: core::sha256::Sha256StateHandle, input: Box<[u32; 16]>,
) -> SyscallResult<core::sha256::Sha256StateHandle> implicits(GasBuiltin, System) nopanic;
