use core::box::Box;
use core::option::OptionTrait;
use core::array::Span;
use core::traits::Into;
use core::traits::TryInto;
use core::zeroable::Zeroable;

/// Store trait and implementations for various types.
pub mod storage_access;
/// Re-imports
pub use storage_access::{Store, StorageAddress};
use storage_access::{
    StorePacking, StorageBaseAddress, storage_base_address_const, storage_base_address_from_felt252,
    storage_address_from_base, storage_address_from_base_and_offset, storage_address_to_felt252,
    storage_address_try_from_felt252
};

/// Module containing all the extern declaration of the syscalls.
pub mod syscalls;
use syscalls::{
    call_contract_syscall, deploy_syscall, emit_event_syscall, get_block_hash_syscall,
    get_execution_info_syscall, library_call_syscall, send_message_to_l1_syscall,
    storage_read_syscall, storage_write_syscall, replace_class_syscall, keccak_syscall
};

/// secp256
pub mod secp256_trait;
pub mod secp256k1;
pub mod secp256r1;

/// ContractAddress
pub mod contract_address;
pub use contract_address::{ContractAddress, contract_address_const};
use contract_address::{
    ContractAddressIntoFelt252, Felt252TryIntoContractAddress, contract_address_to_felt252,
    contract_address_try_from_felt252
};

/// EthAddress
pub mod eth_address;
pub use eth_address::EthAddress;
use eth_address::{
    EthAddressIntoFelt252, EthAddressSerde, EthAddressZeroable, Felt252TryIntoEthAddress
};

/// EthSignature
pub mod eth_signature;
use eth_signature::verify_eth_signature;

/// ClassHash
pub mod class_hash;
pub use class_hash::ClassHash;
use class_hash::{
    ClassHashIntoFelt252, Felt252TryIntoClassHash, class_hash_const, class_hash_to_felt252,
    class_hash_try_from_felt252
};

/// Not `pub` on purpose, only used for direct reexport by the next line.
mod info;
pub use info::{
    v2::ExecutionInfo as ExecutionInfo, BlockInfo, v2::TxInfo as TxInfo, get_execution_info,
    get_caller_address, get_contract_address, get_block_info, get_tx_info, get_block_timestamp,
    get_block_number, v2::ResourceBounds as ResourcesBounds
};

pub mod event;
pub use event::Event;

pub mod account;
pub use account::AccountContract;

/// This module contains the storage-related types and traits for Cairo contracts. It provides
/// abstractions for reading and writing to Starknet storage.
///
/// The front facing interface for the user is simple and intuitive, for example consider the
/// following storage struct:
/// ```
/// #[storage]
/// struct Storage {
///     a: felt252,
///     b: Map<felt252, felt52>,
///     c: Map<felt52, Map<felt52, felt52>>,
/// }
/// ```
/// The user can access the storage members `a` and `b` using the following code:
/// ```
/// fn use_storage(self: @ContractState) {
///     let a_value = self.a.read();
///     // For a Map, the user can use the `entry` method to access the value at a specific key:
///     let b_value = self.b.entry(42).read();
///     // Or simply pass the key to the `read` method:
///     let b_value = self.b.read(42);
///     // Accessing a nested Map must be done using the `entry` method, either:
///     let c_value = self.c.entry(42).entry(43).read()
///     // Or:
///     let c_value = self.c.entry(42).read(43);
/// }
///  ```
///
/// Under the hood, the storage access is more complex. The life cycle of a storage object is as
/// follows:
/// 1. The storage struct of a contract is represented by a `FlattenedStorage` struct, which
///    can be derefed into a struct containing a member for each storage member of the contract.
///    This member can be either a `StorageBase` or a `FlattenedStorage` instance. Members are
///    represented as a `FlattenedStorage` if the storage member is attributed with either
///    `#[substorage(v0)]` (for backward compatibility) or `#[flat]`. `FlattenedStorage` is used to
///    structure the storage access; however, it does not affect the address of the storage object.
/// 2. `StorageBase` members of a `FlattenedStorage` struct hold a single `felt252` value, which is
///    the Keccak hash of the name of the member. For simple types, this value will be the address
///    of the member in the storage.
/// 3. `StorageBase` members are then converted to `StoragePath` instances, which are essentially
///    a wrapper around a `HashState` instance, used to account for more values when computing the
///    address of the storage object. `StoragePath` instances can be updated with values coming from
///    two sources:
///     - Storage nodes, which are structs that represent another struct with all its members
///       in the storage, similar to `FlattenedStorage`. However, unlike `FlattenedStorage`, the
///       path to the storage node does affect the address of the storage object. See `StorageNode`
///       for more details.
///     - Storage collections, specifically `Map` and `Vec`, simulate the behavior of collections by
///       updating the hash state with the key or index of the collection member.
/// 4. After finishing the updates, the `StoragePath` instance is finalized, resulting in a
///    `StoragePointer0Offset` instance, which is a pointer to the address of the storage object. If
///    the pointer is to an object of size greater than 1, the object is stored in a sequential
///    manner starting from the address of the pointer. The whole object can be read or written
///    using `read` and `write` methods, and specific members can also be accessed in the case of a
///    struct. See `SubPointers` for more details.
///
/// The transitioning between the different types of storage objects is also called from the
/// `Deref` trait, and thus, allowing an access to the members of the storage object in a simple
/// way.
///
/// The types mentioned above are generic in the stored object type. This is done to provide
/// specific behavior for each type of stored object, e.g., a `StoragePath` of `Map` type will have
/// an `entry` method, but it won't have a `read` or `write` method, as `Map` is not storable by
/// itself, only its values are.
/// The generic type of the storage object can also be wrapped with a `Mutable` type, which
/// indicates that the storage object is mutable, i.e., it was created from a `ref` contract state,
/// and thus the object can be written to.
pub mod storage;

pub extern type System;

/// An Helper function to force the inclusion of `System` in the list of implicits.
#[deprecated(
    feature: "use_system_implicit",
    note: "Use `core::internal::require_implicit::<System>` instead."
)]
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

/// Module for starknet testing only.
/// Provides functions useful for testing event emission, starknet state information, and the
/// cheatcode concept in general.
pub mod testing;
