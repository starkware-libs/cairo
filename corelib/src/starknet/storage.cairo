//! Storage-related types and traits for Cairo contracts.
//!
//! This module implements the storage system for Starknet contracts, providing high-level
//! abstractions for persistent data storage. It offers a type-safe interface for reading and
//! writing to Starknet storage through the [`StoragePointerReadAccess`] and
//! [`StoragePointerWriteAccess`] traits, along with useful storage-only collection types like
//! [`Vec`] and [`Map`].
//!
//! [`Vec`]: starknet::storage::vec::Vec
//! [`Map`]: starknet::storage::map::Map
//!
//! # Overview
//!
//! The storage system in Starknet contracts is built on a key-value store where each storage slot
//! is identified by a 251-bit address. The storage system allows interactions with storage using
//! state variables, which are declared inside a `Storage` struct annotated with the `#[storage]`
//! attribute. This ensures type-safe storage access and simplifies the process of reading and
//! writing to storage.
//!
//! # Using the Storage System
//!
//! Storage is typically declared using the `#[storage]` attribute on a struct:
//!
//! ```
//! #[storage]
//! struct Storage {
//!     balance: u256,
//!     users: Map<ContractAddress, User>,
//!     nested_data: Map<ContractAddress, Map<ContractAddress, u8>>,
//!     collection: Vec<u8>,
//! }
//! ```
//!
//! Any type that implements the `Store` trait (or it's optimized `StorePacked` variant) can be used
//! in storage.  This type can simply be derived using `#[derive(Store)]` - provided that all of the
//! members of the type also implement `Store`.
//!
//! ```
//! #[derive(Copy, Default, Drop, Store)]
//! struct User {
//!     name: felt252,
//!     age: u8,
//! }
//! ```
//!
//! Interaction with storage is made through a set of traits, depending on the type interacted
//! with:
//!
//! - [`StoragePointerReadAccess`] and [`StoragePointerWriteAccess`] allow for reading and writing
//! storable types.
//! - [`StorageMapReadAccess`] and [`StorageMapWriteAccess`] allow for reading and writing to
//! storage [`Map`]s.
//! - [`StoragePathEntry`] allows for accessing a specific entry in a [`Map`], and can be combined
//! with the `StoragePointer` traits to read and write in these entries.
//! - [`VecTrait`] and [`MutableVecTrait`] allow for interacting with storage [`Vec`]s.
//!
//! [`VecTrait`]: starknet::storage::vec::VecTrait
//! [`MutableVecTrait`]: starknet::storage::vec::MutableVecTrait
//! [`StorageMapReadAccess`]: starknet::storage::map::StorageMapReadAccess
//! [`StorageMapWriteAccess`]: starknet::storage::map::StorageMapWriteAccess
//! [`StoragePathEntry`]: starknet::storage::map::StoragePathEntry
//!
//! ## Examples
//!
//! ```
//! fn use_storage(self: @ContractState) {
//!     let address = 'address'.try_into().unwrap();
//!     // Reading values
//!     let balance = self.balance.read();
//!     // For a `Map`, use the `entry` method to access values at specific keys:
//!     let user = self.users.entry(address).read();
//!     // Accessing nested `Map`s requires chaining `entry` calls:
//!     let nested = self.nested_data.entry(address).entry(address).read();
//!     // Accessing a specific index in a `Vec` requires using the `index` method:
//!     let element = self.collection[index];
//!
//!     // Writing values
//!     self.balance.write(100);
//!     self.users.entry(address).write(Default::default());
//!     self.nested_data.entry(address).entry(address).write(10);
//!     self.collection[index].write(20);
//! }
//! ```
//!
//! # Storage Lifecycle
//!
//! When you access a storage variable, it goes through several transformations:
//!
//! 1. **FlattenedStorage**: The starting point is your contract's storage struct. Each member is
//!    represented either as a `StorageBase` or another `FlattenedStorage` (for `#[substorage(v0)]`
//!    or `#[flat]` members).
//!
//! 2. **StorageBase**: For simple variables, this holds the `sn_keccak` hash of the variable name,
//!    which becomes the storage address. For example:
//!    ```
//!    #[storage]
//!    struct Storage {
//!        balance: u128,  // Stored at sn_keccak('balance')
//!    }
//!    ```
//! 3. **StoragePath**: For complex types, a `StoragePath` represents an un-finalized path to a
//!     specific entry in storage. For example, a `StoragePath` for a `Map` can be updated with
//!     specific keys to point to a specific entry in the map.
//!
//! 4. **StoragePointer**: The final form, pointing to the actual storage location. For multi-slot
//!    values (like structs), values are stored sequentially from this address.
//!
//! # Storage Collections
//!
//! Cairo's memory collection types, like [`Felt252Dict`] and [`Array`], can not be used in storage.
//! Consequently, any type that contains these types can not be used in storage either.
//! Instead, Cairo has two storage-only collection types: [`Map`] and [`Vec`].
//!
//! Instead of storing these _memory_ collections directly, you will need to reflect them into
//! storage using the [`Map`] and [`Vec`] types.
//!
//! # Address Calculation
//!
//! Storage addresses are calculated deterministically:
//!
//! * For a single value variable, the address is the `sn_keccak` hash of the variable name's ASCII
//! encoding. `sn_keccak` is Starknet's version of the Keccak-256 hash function, with its output
//! truncated to 250 bits.
//!
//! * For variables composed of multiple values (tuples, structs, or enums), the base storage
//! address is also the `sn_keccak` hash of the variable name's ASCII encoding. The storage layout
//! then varies depending on the specific type. A struct will store its members as a sequence of
//! primitive types, while an enum will store its variant index, followed by the members of the
//! variant.
//!
//! * For variables within a storage node, the address is calculated using a chain of hashes that
//! represents the node structure. Given a member `m` within a storage variable `variable_name`,
//! the path is computed as `h(sn_keccak(variable_name), sn_keccak(m))`, where `h` is the Pedersen
//! hash. For nested storage nodes, this process repeats, creating a hash chain representing the
//! path to each leaf node. At the leaf node, the storage calculation follows the standard rules for
//! that variable type.
//!
//! * For [`Map`] or [`Vec`] variables, the address is calculated relative to the storage base
//! address (the `sn_keccak` hash of the variable name) combined with the mapping keys or vector
//! indices.
//! See their respective module documentation for more details.
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
use vec::{
    MutableVecIndexView, MutableVecIntoIterRange, PathableMutableVecIntoIterRange,
    PathableVecIntoIterRange, VecIndexView, VecIntoIterRange,
};
pub use vec::{MutableVecTrait, Vec, VecTrait};

/// A pointer to an address in storage, can be used to read and write values, if the generic type
/// supports it (e.g. basic types like `felt252`).
pub struct StoragePointer<T> {
    pub __storage_pointer_address__: StorageBaseAddress,
    pub __storage_pointer_offset__: u8,
}

impl StoragePointerCopy<T> of Copy<StoragePointer<T>> {}
impl StoragePointerDrop<T> of Drop<StoragePointer<T>> {}

/// This makes the sub-pointers members directly accessible from a pointer to the parent struct.
pub impl SubPointersDeref<T, +SubPointers<T>> of core::ops::Deref<StoragePointer<T>> {
    type Target = SubPointers::<T>::SubPointersType;
    fn deref(self: StoragePointer<T>) -> Self::Target {
        self.sub_pointers()
    }
}

/// This makes the sub-pointers members directly accessible from a pointer to the parent struct.
pub impl SubPointersMutDeref<
    T, +SubPointersMut<T>,
> of core::ops::Deref<StoragePointer<Mutable<T>>> {
    type Target = SubPointersMut::<T>::SubPointersType;
    fn deref(self: StoragePointer<Mutable<T>>) -> Self::Target {
        self.sub_pointers_mut()
    }
}


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
//! use starknet::storage::StoragePointerReadAccess;
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
//! use starknet::storage::StoragePointerWriteAccess;
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

/// This makes the storage node members directly accessible from a path to the parent struct.
pub impl StorageNodeDeref<T, +StorageNode<T>> of core::ops::Deref<StoragePath<T>> {
    type Target = StorageNode::<T>::NodeType;
    fn deref(self: StoragePath<T>) -> Self::Target {
        self.storage_node()
    }
}

/// This makes the storage node members directly accessible from a path to the parent struct.
pub impl StorageNodeMutDeref<T, +StorageNodeMut<T>> of core::ops::Deref<StoragePath<Mutable<T>>> {
    type Target = StorageNodeMut::<T>::NodeType;
    fn deref(self: StoragePath<Mutable<T>>) -> Self::Target {
        self.storage_node_mut()
    }
}

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

pub trait StoragePathMutableConversion<T> {
    /// Converts a `StoragePath<Mutable<T>>` to a `StoragePath<T>`. This is useful to expose
    /// functions implemented for `StoragePath<T>` on a `StoragePath<Mutable<T>>`.
    fn as_non_mut(self: StoragePath<Mutable<T>>) -> StoragePath<T>;
}


impl StoragePathAsNonMutImpl<T> of StoragePathMutableConversion<T> {
    fn as_non_mut(self: StoragePath<Mutable<T>>) -> StoragePath<T> {
        StoragePath { __hash_state__: self.__hash_state__ }
    }
}


/// Trait for turning collection of values into an iterator over a specific range.
pub trait IntoIterRange<T> {
    type IntoIter;
    impl Iterator: Iterator<Self::IntoIter>;
    /// Creates an iterator over a range from a collection.
    fn into_iter_range(self: T, range: core::ops::Range<u64>) -> Self::IntoIter;
    /// Creates an iterator over the full range of a collection.
    fn into_iter_full_range(self: T) -> Self::IntoIter;
}

/// Trait that ensures a type is valid for storage in Starknet contracts.
/// This trait is used to enforce that only specific types, such as those implementing
/// `Store` or acting as a `StorageNode`, can be a part of a storage hierarchy. Any type
/// that does not implement this trait cannot be used in a storage struct.
pub trait ValidStorageTypeTrait<T>;

/// Implementation of `ValidStorageTypeTrait` for types that implement `starknet::Store`.
impl ValidStorageTypeTraitStoreImpl<T, +starknet::Store<T>> of ValidStorageTypeTrait<T>;

/// `StorageTrait` is typically used for storage nodes, which help organize contract storage
/// hierarchies. By implementing `ValidStorageTypeTrait`, this ensures that storage nodes (and
/// substorages within components) are valid storage types.
impl ValidStorageTypeTraitStorageNodeImpl<T, +StorageTrait<T>> of ValidStorageTypeTrait<T>;
