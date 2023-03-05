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
use storage_access::storage_address_try_from_felt;

// ContractAddress
mod contract_address;
use contract_address::ContractAddress;
use contract_address::ContractAddressIntoFelt;
use contract_address::FeltTryIntoContractAddress;
use contract_address::contract_address_const;
use contract_address::contract_address_to_felt;
use contract_address::contract_address_try_from_felt;
use contract_address::ContractAddressZeroable;

mod info;
use info::ExecutionInfo;
use info::BlockInfo;
use info::TxInfo;
use info::get_execution_info;
use info::get_caller_address;
use info::get_contract_address;
use info::get_block_info;
use info::get_tx_info;

extern type System;

// An Helper function to force the inclusion of `System` in the list of implicits.
fn use_system_implicit() implicits(System) {}


// Interoperability.
extern fn call_contract_syscall(
    address: ContractAddress, entry_point_selector: felt, calldata: Array<felt>
) -> SyscallResult<Array<felt>> implicits(GasBuiltin, System) nopanic;

// Events.
extern fn emit_event_syscall(
    keys: Array<felt>, data: Array<felt>
) -> SyscallResult<()> implicits(GasBuiltin, System) nopanic;

/// The result type for a syscall.
type SyscallResult<T> = Result<T, Array<felt>>;

trait SyscallResultTrait<T> {
    /// If `val` is `Result::Ok(x)`, returns `x`. Otherwise, panics with the revert reason.
    fn unwrap_syscall(self: SyscallResult<T>) -> T;
}
impl SyscallResultTraitImpl<T> of SyscallResultTrait::<T> {
    fn unwrap_syscall(self: SyscallResult<T>) -> T {
        match self {
            Result::Ok(x) => x,
            Result::Err(revert_reason) => {
                panic(revert_reason)
            },
        }
    }
}

/// The expected return value of the `__validate*__` functions of an accounted contract.
const VALIDATED: felt = 'VALID';

// Module for starknet testing only.
mod testing;
