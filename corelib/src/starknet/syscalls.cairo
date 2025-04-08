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
use starknet::SyscallResult;
use starknet::class_hash::ClassHash;
use starknet::contract_address::ContractAddress;
use starknet::storage_access::StorageAddress;

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
///
/// # Returns
///
/// * The hash of the block with the given number.
pub extern fn get_block_hash_syscall(
    block_number: u64,
) -> SyscallResult<felt252> implicits(GasBuiltin, System) nopanic;

/// Gets information about the currently executing block and the transactions within it.
/// For a complete description of this information, see [`Execution information`].
/// When an account’s `__validate__`, `__validate_deploy__`, or `__validate_declare__` function
///
/// calls `get_execution_info`, the return values for `block_timestamp` and `block_number` are
/// modified as follows:
/// * `block_timestamp` returns the hour, rounded down to the nearest hour.
/// * `block_number` returns the block number, rounded down to the nearest multiple of 100.
///
/// [`Execution information`]: starknet::info::ExecutionInfo
///
/// # Returns
///
/// * A struct that contains information about the currently executing function, transaction, and
/// block.
pub extern fn get_execution_info_syscall() -> SyscallResult<
    Box<starknet::info::ExecutionInfo>,
> implicits(GasBuiltin, System) nopanic;

/// Gets information about the current execution, version 2.
/// This syscall should not be called directly. Instead, use
/// `starknet::info::get_execution_info`.
///
/// # Returns
///
/// * A box containing the current V2 execution information.
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

/// Replaces the class hash of the current contract, instantly modifying its entrypoints.
///
/// The new class becomes effective only after the current function call completes.
/// The remaining code in the current function will continue executing from the old class.
/// The new class will be used:
/// * In subsequent transactions
/// * If the contract is called via `call_contract` syscall later in the same transaction
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
/// # Returns
///
/// * The class hash of the contract's originating code.
pub extern fn get_class_hash_at_syscall(
    contract_address: ContractAddress,
) -> SyscallResult<ClassHash> implicits(GasBuiltin, System) nopanic;

/// Computes the keccak of the input.
///
/// * The input must be a multiple of 1088 bits (== 17 u64 words)
/// * The input must be pre-padded following the Keccak padding rule (pad10*1):
///   1. Add a '1' bit
///   2. Add zero or more '0' bits
///   3. Add a final '1' bit
///   The total length after padding must be a multiple of 1088 bits
///
/// # Arguments
///
/// * `input` - Array of 64-bit words (little endian) to be hashed.
///
/// # Returns
///
/// * The keccak hash as a little-endian u256
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
/// # Returns
///
/// * The next SHA-256 state of the input with the givens state.
///
/// The system call does not add any padding and the input needs to be a multiple of 512 bits
/// (== 16 u32 word).
pub extern fn sha256_process_block_syscall(
    state: core::sha256::Sha256StateHandle, input: Box<[u32; 16]>,
) -> SyscallResult<core::sha256::Sha256StateHandle> implicits(GasBuiltin, System) nopanic;

/// Invokes the given entry point as a v0 meta transaction.
///
/// * The signature is replaced with the given signature.
/// * The caller is the OS (address 0).
/// * The transaction version is replaced by 0.
/// * The transaction hash is replaced by the corresponding version-0 transaction hash.
///
/// The changes apply to the called contract and the inner contracts it calls.
///
/// NOTE: This syscall should only be used to allow support for old version-0 bound accounts,
/// and should not be used for other purposes.
extern fn meta_tx_v0_syscall(
    address: ContractAddress,
    entry_point_selector: felt252,
    calldata: Span<felt252>,
    signature: Span<felt252>,
) -> starknet::SyscallResult<Span<felt252>> implicits(GasBuiltin, System) nopanic;
