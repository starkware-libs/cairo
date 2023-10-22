use box::Box;
use option::OptionTrait;
use array::Span;
use traits::Into;
use traits::TryInto;
use zeroable::Zeroable;

// Re-imports
// Store
mod storage_access;
use storage_access::{
    Store, StorePacking, StorageAddress, StorageBaseAddress, storage_base_address_const,
    storage_base_address_from_felt252, storage_address_from_base,
    storage_address_from_base_and_offset, storage_address_to_felt252,
    storage_address_try_from_felt252
};

// Module containing all the extern declaration of the syscalls.
mod syscalls;
use syscalls::{
    call_contract_syscall, deploy_syscall, emit_event_syscall, get_block_hash_syscall,
    get_execution_info_syscall, library_call_syscall, send_message_to_l1_syscall,
    storage_read_syscall, storage_write_syscall, replace_class_syscall, keccak_syscall
};

// secp256
mod secp256_trait;
mod secp256k1;
mod secp256r1;

// ContractAddress
mod contract_address;
use contract_address::{
    ContractAddress, ContractAddressIntoFelt252, Felt252TryIntoContractAddress,
    contract_address_const, contract_address_to_felt252, contract_address_try_from_felt252
};

// EthAddress
mod eth_address;
use eth_address::{
    EthAddress, EthAddressIntoFelt252, EthAddressSerde, EthAddressZeroable, Felt252TryIntoEthAddress
};

// EthSignature
mod eth_signature;
use eth_signature::verify_eth_signature;

// ClassHash
mod class_hash;
use class_hash::{
    ClassHash, ClassHashIntoFelt252, Felt252TryIntoClassHash, class_hash_const,
    class_hash_to_felt252, class_hash_try_from_felt252
};

mod info;
use info::{
    ExecutionInfo, BlockInfo, TxInfo, get_execution_info, get_caller_address, get_contract_address,
    get_block_info, get_tx_info, get_block_timestamp
};

mod event;
use event::Event;

mod account;
use account::AccountContract;

mod storage;

extern type System;

// An Helper function to force the inclusion of `System` in the list of implicits.
fn use_system_implicit() implicits(System) {}

/// The result type for a syscall.
type SyscallResult<T> = Result<T, Array<felt252>>;

trait SyscallResultTrait<T> {
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
const VALIDATED: felt252 = 'VALID';

// Module for starknet testing only.
mod testing;
