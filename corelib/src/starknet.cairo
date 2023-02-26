use box::Box;
use option::OptionTrait;
use array::Span;
use traits::Into;
use traits::TryInto;
use zeroable::Zeroable;

// Re-imports
// StorageAccess
mod storage_access;
use storage_access::StorageAccess;
use storage_access::StorageAddress;
use storage_access::StorageBaseAddress;
use storage_access::storage_base_address_const;
use storage_access::storage_base_address_from_felt;
use storage_access::storage_read_syscall;
use storage_access::storage_write_syscall;
use storage_access::storage_address_from_base;
use storage_access::storage_address_from_base_and_offset;

// ContractAddress
mod contract_address;
use contract_address::ContractAddress;
use contract_address::ContractAddressIntoFelt;
use contract_address::FeltTryIntoContractAddress;
use contract_address::contract_address_const;
use contract_address::contract_address_to_felt;
use contract_address::contract_address_try_from_felt;
use contract_address::ContractAddressZeroable;
use contract_address::ContractAddressSerde;


extern type System;

#[derive(Copy, Drop)]
struct TxInfo {
    // The version of the transaction. It is fixed (currently, 1) in the OS, and should be
    // signed by the account contract.
    // This field allows invalidating old transactions, whenever the meaning of the other
    // transaction fields is changed (in the OS).
    version: felt,
    // The account contract from which this transaction originates.
    account_contract_address: ContractAddress,
    // The max_fee field of the transaction.
    max_fee: u128,
    // The signature of the transaction.
    signature: Span::<felt>,
    // The hash of the transaction.
    transaction_hash: felt,
    // The identifier of the chain.
    // This field can be used to prevent replay of testnet transactions on mainnet.
    chain_id: felt,
    // The transaction's nonce.
    nonce: felt,
}

// An Helper function to force the inclusion of `System` in the list of implicits.
fn use_system_implicit() implicits(System) {}


// Interoperability.
extern fn call_contract_syscall(
    address: ContractAddress, entry_point_selector: felt, calldata: Array::<felt>
) -> SyscallResult::<Array::<felt>> implicits(GasBuiltin, System) nopanic;

// Events.
extern fn emit_event_syscall(
    keys: Array::<felt>, data: Array::<felt>
) -> SyscallResult::<()> implicits(GasBuiltin, System) nopanic;

// Getters.
extern fn get_caller_address_syscall() -> SyscallResult::<ContractAddress> implicits(
    GasBuiltin, System
) nopanic;

fn get_caller_address() -> ContractAddress {
    get_caller_address_syscall().unwrap_syscall()
}

extern fn get_contract_address_syscall() -> SyscallResult::<ContractAddress> implicits(
    GasBuiltin, System
) nopanic;

fn get_contract_address() -> ContractAddress {
    get_contract_address_syscall().unwrap_syscall()
}

extern fn get_sequencer_address_syscall() -> SyscallResult::<ContractAddress> implicits(
    GasBuiltin, System
) nopanic;

fn get_sequencer_address() -> ContractAddress {
    get_sequencer_address_syscall().unwrap_syscall()
}

extern fn get_block_number_syscall() -> SyscallResult::<u64> implicits(GasBuiltin, System) nopanic;

fn get_block_number() -> u64 {
    get_block_number_syscall().unwrap_syscall()
}

extern fn get_tx_info_syscall() -> SyscallResult::<Box::<TxInfo>> implicits(
    GasBuiltin, System
) nopanic;

fn get_tx_info() -> Box::<TxInfo> {
    get_tx_info_syscall().unwrap_syscall()
}

extern fn get_block_timestamp_syscall() -> SyscallResult::<u64> implicits(
    GasBuiltin, System
) nopanic;

// TODO(ilya): Consider Adding a type for timestamps.
fn get_block_timestamp() -> u64 {
    get_block_timestamp_syscall().unwrap_syscall()
}

/// The result type for a syscall.
type SyscallResult<T> = Result::<T, Array::<felt>>;

trait SyscallResultTrait<T> {
    /// If `val` is `Result::Ok(x)`, returns `x`. Otherwise, panics with the revert reason.
    fn unwrap_syscall(self: SyscallResult::<T>) -> T;
}
impl SyscallResultTraitImpl<T> of SyscallResultTrait::<T> {
    fn unwrap_syscall(self: SyscallResult::<T>) -> T {
        match self {
            Result::Ok(x) => x,
            Result::Err(revert_reason) => {
                panic(revert_reason)
            },
        }
    }
}
