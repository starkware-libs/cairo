//! Functionalities for interacting with the Starknet network.
//!
//! # Core Components
//!
//! - **Storage**: The `storage` module defines abstractions on how to interact with Starknet
//! contract storage.
//! - **Syscalls**: The `syscalls` module contains the extern declarations for all the system calls
//! available in Starknet.
//! - **Contract Addresses**: The `contract_address` and `eth_address` modules provide types and
//! utilities for working with Starknet contract addresses and Ethereum addresses.
//! - **Cryptography**: The `secp256k1`, `secp256r1`, `secp256_trait`, and `eth_signature` modules
//! handle various elliptic curve operations.
//! - **Execution Info**: The `info` module exposes functions for accessing information about the
//! current contract execution, such as the caller address, contract address, block info, and
//! transaction info.

#[allow(unused_imports)]
use core::array::Span;
#[allow(unused_imports)]
use core::box::Box;
#[allow(unused_imports)]
use core::option::OptionTrait;
#[allow(unused_imports)]
use core::traits::{Into, TryInto};
#[allow(unused_imports)]
use core::zeroable::Zeroable;

pub mod storage_access;
pub use storage_access::{StorageAddress, Store};
#[allow(unused_imports)]
use storage_access::{
    StorageBaseAddress, StorePacking, storage_address_from_base,
    storage_address_from_base_and_offset, storage_address_to_felt252,
    storage_address_try_from_felt252, storage_base_address_const, storage_base_address_from_felt252,
};

pub mod syscalls;
#[allow(unused_imports)]
use syscalls::{
    call_contract_syscall, deploy_syscall, emit_event_syscall, get_block_hash_syscall,
    get_class_hash_at_syscall, get_execution_info_syscall, keccak_syscall, library_call_syscall,
    replace_class_syscall, send_message_to_l1_syscall, storage_read_syscall, storage_write_syscall,
};

pub mod contract_address;

pub mod secp256_trait;
pub mod secp256k1;
pub mod secp256r1;
pub use contract_address::ContractAddress;
#[deprecated(
    feature: "deprecated-starknet-consts",
    note: "Use `TryInto::try_into` in const context instead.",
)]
#[feature("deprecated-starknet-consts")]
pub use contract_address::contract_address_const;
#[allow(unused_imports)]
use contract_address::{
    ContractAddressIntoFelt252, Felt252TryIntoContractAddress, contract_address_to_felt252,
    contract_address_try_from_felt252,
};

pub mod eth_address;
pub use eth_address::EthAddress;
#[allow(unused_imports)]
use eth_address::{
    EthAddressIntoFelt252, EthAddressSerde, EthAddressZeroable, Felt252TryIntoEthAddress,
};

pub mod eth_signature;
#[allow(unused_imports)]
use eth_signature::verify_eth_signature;

pub mod class_hash;
pub use class_hash::ClassHash;
#[allow(unused_imports)]
#[deprecated(
    feature: "deprecated-starknet-consts",
    note: "Use `TryInto::try_into` in const context instead.",
)]
#[feature("deprecated-starknet-consts")]
use class_hash::class_hash_const;
#[allow(unused_imports)]
use class_hash::{
    ClassHashIntoFelt252, Felt252TryIntoClassHash, class_hash_to_felt252,
    class_hash_try_from_felt252,
};

// Not `pub` on purpose, only used for direct reexport by the next line.
mod info;
pub use info::v2::{ExecutionInfo, ResourceBounds as ResourcesBounds, TxInfo};
pub use info::{
    BlockInfo, get_block_info, get_block_number, get_block_timestamp, get_caller_address,
    get_contract_address, get_execution_info, get_tx_info,
};

pub mod event;
pub use event::Event;

pub mod account;
pub use account::AccountContract;

pub mod storage;

pub extern type System;

// A helper function to force the inclusion of `System` in the list of implicits.
#[deprecated(
    feature: "use_system_implicit",
    note: "Use `core::internal::require_implicit::<System>` instead.",
)]
fn use_system_implicit() implicits(System) {}

/// The `Result` type for a syscall.
pub type SyscallResult<T> = Result<T, Array<felt252>>;

/// Trait for handling syscall results.
pub trait SyscallResultTrait<T> {
    /// Unwraps a syscall result, yielding the content of an `Ok`.
    ///
    /// # Panics
    ///
    /// Panics with the syscall error message if the value is an `Err`.
    ///
    /// # Examples
    ///
    /// ```
    /// let result = starknet::syscalls::get_execution_info_v2_syscall();
    /// let info = result.unwrap_syscall();
    /// ```
    fn unwrap_syscall(self: SyscallResult<T>) -> T;
}

impl SyscallResultTraitImpl<T> of SyscallResultTrait<T> {
    fn unwrap_syscall(self: SyscallResult<T>) -> T {
        match self {
            Ok(x) => x,
            Err(revert_reason) => panic(revert_reason),
        }
    }
}

/// The expected return value of the `__validate__` function in account contracts.
///
/// This constant is used to indicate that a transaction validation was successful.
/// Account contracts must return this value from their `__validate__` function to
/// signal that the transaction should proceed.
pub const VALIDATED: felt252 = 'VALID';

pub mod testing;
