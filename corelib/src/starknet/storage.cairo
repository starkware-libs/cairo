//! Storage-related types and traits for Cairo contracts.
//!
//! This module implements a storage system for Starknet contracts, providing high-level
//! abstractions for persistent data storage with deterministic address calculation. It also
//! provides abstractions for reading and writing to Starknet storage with
//! [`StoragePointerReadAccess`] and [`StoragePointerWriteAccess`] traits, respectively.
//!
//! # Storage System Overview
//!
//! Storage in Starknet contracts is managed through a hash-based addressing scheme that ensures
//! deterministic and collision-resistant storage locations. The module provides intuitive
//! high-level abstractions while maintaining security and efficiency.
//!
//! ## Basic Usage
//!
//! Storage is typically declared using the `#[storage]` attribute on a struct:
//!
//! ```
//! #[storage]
//! struct Storage {
//!     balance: felt252,
//!     users: Map<felt252, User>,
//!     nested_data: Map<felt252, Map<felt252, felt252>>,
//! }
//! ```
//!
//! Accessing storage members is straightforward:
//!
//! ```
//! fn use_storage(self: @ContractState, ...) {
//!     // Reading values
//!     let balance = self.balance.read();
//!     // For a `Map`, the user can use the `entry` method to access the value at a specific key:
//!     let user = self.users.entry(user_id).read();
//!     // Accessing a nested `Map` must be done using the `entry` method multiple times:
//!     let nested = self.nested_data.entry(key1).entry(key2).read();
//!
//!     // Writing values
//!     self.balance.write(new_balance);
//!     self.users.entry(user_id).write(new_user);
//!     self.nested_data.entry(key1).entry(key2).write(new_value);
//! }
//! ```
//!
//! # Implementation Details
//!
//! ## Storage Layout
//!
//! The storage system is built on several abstraction layers:
//!
//! 1. `FlattenedStorage`: The top-level representation of contract storage that contains either
//!    `StorageBase` instances or nested `FlattenedStorage` instances. It handles storage
//!    organization based on attributes like `#[substorage(v0)]` or `#[flat]`.
//!
//! 2. `StorageBase`: Represents single `felt252` values, using the Keccak hash of the member's
//!    name as the storage address.
//!
//! 3. `StoragePath`: `StorageBase` members are converted to `StoragePath` instances, which are
//!    essentially a wrapper around `HashState` that accumulates components for final address
//!    calculation, supporting both simple and complex storage types.
//!
//! 4. `StoragePointer`: The final representation pointing to a storage location, supporting both
//!    direct and offset-based access for reading and writing values.
//!
//! ## Address Calculation
//!
//! Storage addresses are calculated deterministically:
//!
//! * If the variable is a single value, the address is the `sn_keccak` hash of the ASCII encoding
//! of the variable's name. `sn_keccak` is Starknet's version of the Keccak-256 hash function, whose
//! output is truncated to 250 bits.
//!
//! * If the variable is composed of multiple values (i.e., a tuple, a struct or an enum), we also
//! use the `sn_keccak` hash of the ASCII encoding of the variable's name to determine the base
//! address in storage. Then, depending on the type, the storage layout will differ.
//!
//! * If the variable is part of a storage node, its address is based on a chain of hashes that
//! reflects the structure of the node. For a storage node member m within a storage variable
//! variable_name, the path to that member is computed as h(sn_keccak(variable_name), sn_keccak(m)),
//! where h is the Pedersen hash. This process continues for nested storage nodes, building a chain
//! of hashes that represents the path to a leaf node. Once a leaf node is reached, the storage
//! calculation proceeds as it normally would for that type of variable.
//!
//! * If the variable is a `Map` or a `Vec`, the address is computed relative to the storage base
//! address, which is the `sn_keccak` hash of the variable's name, and the keys of the mapping or
//! indexes in the `Vec`.
//!
//! ## Storage Collections
//!
//! The module provides two primary collection types:
//!
//! ### Maps
//! - Key-value storage with deterministic addressing
//! - Support for nested mappings
//! - Efficient key-based lookups
//!
//! ### Vectors
//! - Sequential storage with dynamic size
//! - Index-based address calculation
//! - Automatic length management

use core::hash::HashStateTrait;
#[allow(unused_imports)]
use core::pedersen::HashState;
use core::traits::Into;
#[allow(unused_imports)]
use starknet::SyscallResult;
use starknet::storage_access::{StorageBaseAddress, storage_base_address_from_felt252};

mod map;
pub use map::{Map, StorageMapReadAccess, StorageMapWriteAccess, StoragePathEntry};

mod storage_base;
pub use storage_base::{FlattenedStorage, StorageBase, StorageTrait, StorageTraitMut};

mod storage_node;
pub use storage_node::{StorageNode, StorageNodeMut};

mod sub_pointers;
pub use sub_pointers::{SubPointers, SubPointersForward, SubPointersMut, SubPointersMutForward};

mod vec;
use vec::{MutableVecIndexView, VecIndexView};
pub use vec::{MutableVecTrait, Vec, VecTrait};

/// A pointer to an address in storage, can be used to read and write values, if the generic type
/// supports it (e.g. basic types like `felt252`).
pub struct StoragePointer<T> {
    pub __storage_pointer_address__: StorageBaseAddress,
    pub __storage_pointer_offset__: u8,
}

impl StoragePointerCopy<T> of Copy<StoragePointer<T>> {}
impl StoragePointerDrop<T> of Drop<StoragePointer<T>> {}

/// StoragePointer can be dereferenced into a sub-pointers type, this import allows the impl to be
/// found next to the type.
use sub_pointers::{SubPointersDeref, SubPointersMutDeref};


/// Same as `StoragePointer`, but with `offset` 0, which allows for some optimizations.
pub struct StoragePointer0Offset<T> {
    pub __storage_pointer_address__: StorageBaseAddress,
}

impl StoragePointer0OffsetCopy<T> of Copy<StoragePointer0Offset<T>> {}
impl StoragePointer0OffsetDrop<T> of Drop<StoragePointer0Offset<T>> {}

/// Trait for converting a storage member to a `StoragePointer0Offset`.
// type instead of `T`.
pub trait StorageAsPointer<TMemberState> {
    type Value;
    fn as_ptr(self: @TMemberState) -> StoragePointer0Offset<Self::Value>;
}

/// Trait for accessing the values in storage using a `StoragePointer`.
///
/// # Examples
///
//! ```
//! use core::starknet::storage::StoragePointerReadAccess;
//!
//! #[storage]
//! struct Storage {
//!     element: felt252,
//! }
//!
//! fn read_storage(self: @ContractState) -> felt252 {
//!     self.element.read()
//! }
//! ```
pub trait StoragePointerReadAccess<T> {
    type Value;
    fn read(self: @T) -> Self::Value;
}

/// Trait for writing values to storage using a `StoragePointer`.
///
/// # Examples
///
//! ```
//! use core::starknet::storage::StoragePointerWriteAccess;
//!
//! #[storage]
//! struct Storage {
//!     element: felt252,
//! }
//!
//! fn write_storage(self: @ContractState) {
//!     self.element.write(1);
//! }
//! ```
pub trait StoragePointerWriteAccess<T> {
    type Value;
    fn write(self: T, value: Self::Value);
}

/// Simple implementation of `StoragePointerReadAccess` for any type that implements `Store` for 0
/// offset.
impl StorableStoragePointer0OffsetReadAccess<
    T, +starknet::Store<T>,
> of StoragePointerReadAccess<StoragePointer0Offset<T>> {
    type Value = T;
    fn read(self: @StoragePointer0Offset<T>) -> T {
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<T>::read(0, *self.__storage_pointer_address__),
        )
    }
}

/// Simple implementation of `StoragePointerReadAccess` for any mutable type that implements `Store`
/// for 0 offset.
impl MutableStorableStoragePointer0OffsetReadAccess<
    T, +MutableTrait<T>, +starknet::Store<MutableTrait::<T>::InnerType>,
> of StoragePointerReadAccess<StoragePointer0Offset<T>> {
    type Value = MutableTrait::<T>::InnerType;
    fn read(self: @StoragePointer0Offset<T>) -> MutableTrait::<T>::InnerType {
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<
                MutableTrait::<T>::InnerType,
            >::read(0, *self.__storage_pointer_address__),
        )
    }
}

/// Simple implementation of `StoragePointerWriteAccess` for any mutable type that implements
/// `Store` for 0 offset.
impl StorableStoragePointer0OffsetWriteAccess<
    T, +MutableTrait<T>, +starknet::Store<MutableTrait::<T>::InnerType>,
> of StoragePointerWriteAccess<StoragePointer0Offset<T>> {
    type Value = MutableTrait::<T>::InnerType;
    fn write(self: StoragePointer0Offset<T>, value: MutableTrait::<T>::InnerType) {
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<
                MutableTrait::<T>::InnerType,
            >::write(0, self.__storage_pointer_address__, value),
        )
    }
}

/// Simple implementation of `StoragePointerReadAccess` for any type that implements `Store` for any
/// offset.
pub impl StorableStoragePointerReadAccess<
    T, +starknet::Store<T>,
> of StoragePointerReadAccess<StoragePointer<T>> {
    type Value = T;
    fn read(self: @StoragePointer<T>) -> T {
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<
                T,
            >::read_at_offset(
                0, *self.__storage_pointer_address__, *self.__storage_pointer_offset__,
            ),
        )
    }
}

/// Simple implementation of `StoragePointerReadAccess` for any mutable type that implements `Store`
/// for any offset.
impl MutableStorableStoragePointerReadAccess<
    T, +MutableTrait<T>, +starknet::Store<MutableTrait::<T>::InnerType>,
> of StoragePointerReadAccess<StoragePointer<T>> {
    type Value = MutableTrait::<T>::InnerType;
    fn read(self: @StoragePointer<T>) -> MutableTrait::<T>::InnerType {
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<
                MutableTrait::<T>::InnerType,
            >::read_at_offset(
                0, *self.__storage_pointer_address__, *self.__storage_pointer_offset__,
            ),
        )
    }
}

/// Simple implementation of `StoragePointerWriteAccess` for any mutable type that implements
/// `Store` for any offset.
impl MutableStorableStoragePointerWriteAccess<
    T, +MutableTrait<T>, +starknet::Store<MutableTrait::<T>::InnerType>,
> of StoragePointerWriteAccess<StoragePointer<T>> {
    type Value = MutableTrait::<T>::InnerType;
    fn write(self: StoragePointer<T>, value: MutableTrait::<T>::InnerType) {
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<
                MutableTrait::<T>::InnerType,
            >::write_at_offset(
                0, self.__storage_pointer_address__, self.__storage_pointer_offset__, value,
            ),
        )
    }
}

/// An intermediate struct to store a hash state, in order to be able to hash multiple values and
/// get the final address.
/// Storage path should have two interfaces, if `T` is storable then it should implement
/// `StorageAsPointer` in order to be able to get the address of the storage path. Otherwise, if
/// `T` is not storable then it should implement some kind of updating trait, e.g.
/// `StoragePathEntry`.
pub struct StoragePath<T> {
    __hash_state__: StoragePathHashState,
}

/// The hash state of a storage path.
type StoragePathHashState = core::pedersen::HashState;

impl StoragePathCopy<T> of core::traits::Copy<StoragePath<T>> {}
impl StoragePathDrop<T> of core::traits::Drop<StoragePath<T>> {}

/// StoragePath can be dereferenced into a storage node, this import allows the impl to be found
/// next to the type.
use storage_node::{StorageNodeDeref, StorageNodeMutDeref};

/// Trait for StoragePath operations.
trait StoragePathTrait<T> {
    fn new(init_value: felt252) -> StoragePath<T>;
    fn finalize(self: StoragePath<T>) -> StorageBaseAddress;
}

impl StoragePathImpl<T> of StoragePathTrait<T> {
    fn new(init_value: felt252) -> StoragePath<T> {
        StoragePath { __hash_state__: core::pedersen::PedersenTrait::new(init_value) }
    }

    fn finalize(self: StoragePath<T>) -> StorageBaseAddress {
        storage_base_address_from_felt252(self.__hash_state__.finalize())
    }
}

/// Trait for updating the hash state of a storage path with a given value. Also changes the generic
/// type of the storage path from `SourceType` to `TargetType`.
trait StoragePathUpdateTrait<SourceType, TargetType, Value> {
    fn update(self: StoragePath<SourceType>, value: Value) -> StoragePath<TargetType>;
}

impl StoragePathUpdateImpl<
    SourceType, TargetType, Value, impl HashImpl: core::hash::Hash<Value, StoragePathHashState>,
> of StoragePathUpdateTrait<SourceType, TargetType, Value> {
    fn update(self: StoragePath<SourceType>, value: Value) -> StoragePath<TargetType> {
        StoragePath { __hash_state__: HashImpl::update_state(self.__hash_state__, value) }
    }
}

impl StoragePathSIntoStoragePathTImpl<
    SourceType, TargetType,
> of Into<StoragePath<SourceType>, StoragePath<TargetType>> {
    fn into(self: StoragePath<SourceType>) -> StoragePath<TargetType> {
        StoragePath { __hash_state__: self.__hash_state__ }
    }
}

/// Trait for creating a new `StoragePath` from a storage member.
pub trait StorageAsPath<TMemberState> {
    type Value;
    fn as_path(self: @TMemberState) -> StoragePath<Self::Value>;
}

/// An implementation of `StorageAsPointer` for any `StoragePath` with inner type that implements
/// `Store`.
impl StorableStoragePathAsPointer<T, +starknet::Store<T>> of StorageAsPointer<StoragePath<T>> {
    type Value = T;
    fn as_ptr(self: @StoragePath<T>) -> StoragePointer0Offset<T> {
        StoragePointer0Offset { __storage_pointer_address__: (*self).finalize() }
    }
}

/// An implementation of `StorageAsPointer` for any `StoragePath` with inner type that implements
/// `Store`.
impl MutableStorableStoragePathAsPointer<
    T, +MutableTrait<T>, +starknet::Store<MutableTrait::<T>::InnerType>,
> of StorageAsPointer<StoragePath<T>> {
    type Value = T;
    fn as_ptr(self: @StoragePath<T>) -> StoragePointer0Offset<T> {
        StoragePointer0Offset { __storage_pointer_address__: (*self).finalize() }
    }
}

/// Implement `Deref` for storage paths that implements `StorageAsPointer`.
impl StoragePathDeref<
    T, impl PointerImpl: StorageAsPointer<StoragePath<T>>,
> of core::ops::Deref<StoragePath<T>> {
    type Target = StoragePointer0Offset<PointerImpl::Value>;
    fn deref(self: StoragePath<T>) -> StoragePointer0Offset<PointerImpl::Value> {
        self.as_ptr()
    }
}

/// Implement `Deref` for `StoragePointer0Offset` into a `StoragePointer`.
impl StoragePointer0OffsetDeref<T> of core::ops::Deref<StoragePointer0Offset<T>> {
    type Target = StoragePointer<T>;
    fn deref(self: StoragePointer0Offset<T>) -> StoragePointer<T> {
        StoragePointer::<
            T,
        > {
            __storage_pointer_address__: self.__storage_pointer_address__,
            __storage_pointer_offset__: 0,
        }
    }
}

/// A struct for delaying the creation of a storage path, used for lazy evaluation in storage nodes.
pub struct PendingStoragePath<T> {
    __hash_state__: StoragePathHashState,
    __pending_key__: felt252,
}

/// A trait for creating a `PendingStoragePath` from a `StoragePath` hash state and a key.
pub trait PendingStoragePathTrait<T, S> {
    fn new(storage_path: @StoragePath<S>, pending_key: felt252) -> PendingStoragePath<T>;
}

impl PendingStoragePathImpl<T, S> of PendingStoragePathTrait<T, S> {
    fn new(storage_path: @StoragePath<S>, pending_key: felt252) -> PendingStoragePath<T> {
        PendingStoragePath {
            __hash_state__: *storage_path.__hash_state__, __pending_key__: pending_key,
        }
    }
}

impl PendingStoragePathDrop<T> of Drop<PendingStoragePath<T>> {}
impl PendingStoragePathCopy<T> of Copy<PendingStoragePath<T>> {}

/// An implementation of 'StorageAsPath' for `PendingStoragePath`.
impl PendingStoragePathAsPath<T> of StorageAsPath<PendingStoragePath<T>> {
    type Value = T;
    fn as_path(self: @PendingStoragePath<T>) -> StoragePath<T> {
        StoragePath::<
            T,
        > {
            __hash_state__: core::hash::HashStateTrait::update(
                *self.__hash_state__, *self.__pending_key__,
            ),
        }
    }
}

/// Deref pending storage path into a storage path.
impl PendingStoragePathDeref<T> of core::ops::Deref<PendingStoragePath<T>> {
    type Target = StoragePath<T>;
    fn deref(self: PendingStoragePath<T>) -> Self::Target {
        self.as_path()
    }
}

/// Implement as_ptr for any type that implements StorageAsPath and Store.
impl StorablePathableStorageAsPointer<
    T,
    impl PathImpl: StorageAsPath<T>,
    impl PtrImpl: StorageAsPointer<StoragePath<PathImpl::Value>>,
> of StorageAsPointer<T> {
    type Value = PtrImpl::Value;
    fn as_ptr(self: @T) -> StoragePointer0Offset<PtrImpl::Value> {
        let path = self.as_path();
        path.as_ptr()
    }
}

/// Implement StoragePointerReadAccess for any type that implements StorageAsPointer and
/// StoragePointerReadAccess.
impl StorablePointerReadAccessImpl<
    T,
    impl PointerImpl: StorageAsPointer<T>,
    impl AccessImpl: StoragePointerReadAccess<StoragePointer0Offset<PointerImpl::Value>>,
> of StoragePointerReadAccess<T> {
    type Value = AccessImpl::Value;
    fn read(self: @T) -> Self::Value {
        self.as_ptr().read()
    }
}

/// Implement StoragePointerWriteAccess for any type that implements StorageAsPointer.
impl StorablePointerWriteAccessImpl<
    T,
    impl PointerImpl: StorageAsPointer<T>,
    impl AccessImpl: StoragePointerWriteAccess<StoragePointer0Offset<PointerImpl::Value>>,
    +Drop<T>,
    +Drop<AccessImpl::Value>,
> of StoragePointerWriteAccess<T> {
    type Value = AccessImpl::Value;
    fn write(self: T, value: Self::Value) {
        let ptr: StoragePointer0Offset<PointerImpl::Value> = self.as_ptr();
        ptr.write(value)
    }
}

/// A wrapper around different storage related types, indicating that the instance is mutable,
/// i.e. originally created from a `ref` contract state.
#[phantom]
pub struct Mutable<T> {}

impl MutableDrop<T> of Drop<Mutable<T>> {}
impl MutableCopy<T> of Copy<Mutable<T>> {}


/// A trait for exposing the inner type of a `Mutable` type.
trait MutableTrait<T> {
    type InnerType;
}

impl MutableImpl<T> of MutableTrait<Mutable<T>> {
    type InnerType = T;
}
