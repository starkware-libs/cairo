use core::box::Box;
use core::option::OptionTrait;
use core::array::Span;
use core::traits::Into;
use core::traits::TryInto;
use core::zeroable::Zeroable;

// Re-imports
// Store
pub mod storage_access;
pub use storage_access::{Store, StorageAddress};
use storage_access::{
    StorePacking, StorageBaseAddress, storage_base_address_const, storage_base_address_from_felt252,
    storage_address_from_base, storage_address_from_base_and_offset, storage_address_to_felt252,
    storage_address_try_from_felt252
};

// Module containing all the extern declaration of the syscalls.
pub mod syscalls;
use syscalls::{
    call_contract_syscall, deploy_syscall, emit_event_syscall, get_block_hash_syscall,
    get_execution_info_syscall, library_call_syscall, send_message_to_l1_syscall,
    storage_read_syscall, storage_write_syscall, replace_class_syscall, keccak_syscall
};

// secp256
pub mod secp256_trait;
pub mod secp256k1;
pub mod secp256r1;

// ContractAddress
pub mod contract_address;
pub use contract_address::{ContractAddress, contract_address_const};
use contract_address::{
    ContractAddressIntoFelt252, Felt252TryIntoContractAddress, contract_address_to_felt252,
    contract_address_try_from_felt252
};

// EthAddress
pub mod eth_address;
pub use eth_address::EthAddress;
use eth_address::{
    EthAddressIntoFelt252, EthAddressSerde, EthAddressZeroable, Felt252TryIntoEthAddress
};

// EthSignature
pub mod eth_signature;
use eth_signature::verify_eth_signature;

// ClassHash
pub mod class_hash;
pub use class_hash::ClassHash;
use class_hash::{
    ClassHashIntoFelt252, Felt252TryIntoClassHash, class_hash_const, class_hash_to_felt252,
    class_hash_try_from_felt252
};

// Not `pub` on purpose, only used for direct reexport by the next line.
mod info;
pub use info::{
    v2::ExecutionInfo as ExecutionInfo, BlockInfo, v2::TxInfo as TxInfo, get_execution_info,
    get_caller_address, get_contract_address, get_block_info, get_tx_info, get_block_timestamp,
    get_block_number
};

pub mod event;
pub use event::Event;

pub mod account;
pub use account::AccountContract;

pub mod storage;

pub extern type System;

// An Helper function to force the inclusion of `System` in the list of implicits.
fn use_system_implicit() implicits(System) {}

/// The result type for a syscall.
pub type SyscallResult<T> = Result<T, Array<felt252>>;

pub trait SyscallResultTrait<T> {
    /// If `val` is `Result::Ok(x)`, returns `x`. Otherwise, panics with the revert reason.
    fn unwrap_syscall(self: SyscallResult<T>) -> T;
}
impl SyscallResultTraitImpl<T> of SyscallResultTrait<T> {
    fn unwrap_syscall(self: SyscallResult<T>) -> T {
        match self {
            Result::Ok(x) => x,
            Result::Err(revert_reason) => panic(revert_reason),
        }
    }
}

/// The expected return value of the `__validate*__` functions of an accounted contract.
pub const VALIDATED: felt252 = 'VALID';

// Module for starknet testing only.
pub mod testing;
