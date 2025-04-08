//! Key-value storage mapping implementation for Starknet contracts.
//!
//! This module provides the core mapping functionality used in Starknet smart contracts,
//! enabling persistent key-value storage. Unlike traditional hash tables, storage mappings
//! do not store the key data itself. Instead, they use the hash of the key to compute
//! a storage slot address where the corresponding value is stored.
//!
//! # Interacting with [`Map`]
//!
//! Storage maps can be accessed through two sets of traits, each serving different use cases:
//!
//! 1. Direct access using `StorageMapReadAccess`/`StorageMapWriteAccess`:
//!    These traits allow you to read from or write to a map directly by providing the key(s)
//!    and value:
//!    ```
//!    // Read directly with key
//!    let value = self.my_map.read(key);
//!
//!    // Write directly with key and value
//!    self.my_map.write(key, value);
//!    ```
//!
//! 2. Path-based access combining `StoragePathEntry` with
//! `StoragePointerReadAccess`/`StoragePointerWriteAccess`:
//!    This approach first computes a `StoragePath` for the entry, which can then be used with
//!    the `StoragePointer` access traits from `starknet::storage`:
//!    ```
//!    // Get storage path for the entry
//!    let path = self.my_map.entry(key);
//!
//!    // Read/write using the storage pointer traits
//!    let value = path.read();
//!    path.write(new_value);
//!    ```
//!
//! The path-based approach is particularly useful for:
//! - Nested mappings where you need to chain multiple keys
//! - Cases where you need to reuse the same storage path multiple times
//!
//! # Storage Address Computation
//!
//! Storage addresses for mapping entries are deterministically computed using hash functions:
//!
//! * For a single key mapping:
//!   ```text
//!   address = h(sn_keccak(variable_name), k) mod N
//!   ```
//!   where:
//!   - `h` is the Pedersen hash function
//!   - `k` is the key value
//!   - `N` is 2^251 - 256
//!
//! * For nested mappings with multiple keys:
//!   ```text
//!   address = h(h(...h(h(sn_keccak(variable_name), k₁), k₂)...), kₙ) mod N
//!   ```
//!   where each key `kᵢ` is hashed sequentially with the result of the previous hash.
//!
//! # Examples
//!
//! Basic usage with a single mapping:
//!
//! ```
//! use starknet::ContractAddress;
//! use starknet::storage::{Map, StorageMapReadAccess, StoragePathEntry,
//! StoragePointerReadAccess};
//!
//! #[storage]
//! struct Storage {
//!     balances: Map<ContractAddress, u256>,
//! }
//!
//! fn read_storage(self: @ContractState, address: ContractAddress) {
//!     let balance = self.balances.read(address);
//!     let balance = self.balances.entry(address).read();
//! }
//! ```
//!
//! Nested mappings:
//!
//! ```
//! #[storage]
//! struct Storage {
//!     allowances: Map<ContractAddress, Map<ContractAddress, u256>>,
//! }
//!
//! fn read_storage(self: @ContractState, owner: ContractAddress, spender: ContractAddress) {
//!     let allowance = self.allowances.entry(owner).entry(spender).read();
//!     let allowance = self.allowances.entry(owner).read(spender);
//! }
//! ```

#[allow(unused_imports)]
use super::{
    Mutable, MutableTrait, StorageAsPath, StorageAsPointer, StoragePath, StoragePathHashState,
    StoragePathTrait, StoragePathUpdateTrait, StoragePointerReadAccess, StoragePointerWriteAccess,
};

/// Provides direct read access to values in a storage [`Map`].
///
/// # Examples
///
/// ```
/// use starknet::ContractAddress;
/// use starknet::storage::{Map, StorageMapReadAccess, StoragePathEntry};
///
/// #[storage]
/// struct Storage {
///     balances: Map<ContractAddress, u256>,
///     allowances: Map<ContractAddress, Map<ContractAddress, u256>>,
/// }
///
/// fn read_storage(self: @ContractState, address: ContractAddress) {
///     // Read from single mapping
///     let balance = self.balances.read(address);
///     // Read from nested mapping
///     let allowance = self.allowances.entry(owner).read(spender);
/// }
/// ```
pub trait StorageMapReadAccess<TMemberState> {
    type Key;
    type Value;
    fn read(self: TMemberState, key: Self::Key) -> Self::Value;
}

/// Provides direct write access to values in a storage [`Map`].
///
/// Enables directly storing values in the contract's storage at the address of the given key.
///
/// # Examples
///
/// ```
/// use starknet::ContractAddress;
/// use starknet::storage::{Map, StorageMapWriteAccess, StoragePathEntry};
///
/// #[storage]
/// struct Storage {
///     balances: Map<ContractAddress, u256>,
///     allowances: Map<ContractAddress, Map<ContractAddress, u256>>,
/// }
///
/// fn write_storage(ref self: ContractState, address: ContractAddress) {
///     // Write to single mapping
///     self.balances.write(address, 100);
///     // Write to nested mapping
///     self.allowances.entry(owner).write(spender, 50);
/// }
/// ```
pub trait StorageMapWriteAccess<TMemberState> {
    type Key;
    type Value;
    fn write(self: TMemberState, key: Self::Key, value: Self::Value);
}

/// Computes storage paths for accessing [`Map`] entries.
///
/// The storage path combines the variable's base path with the key's hash to create a unique
/// identifier for the storage slot. This path can then be used for subsequent read or write
/// operations, or advanced further by chaining the `entry` method.
///
/// # Examples
///
/// ```
/// use starknet::ContractAddress;
/// use starknet::storage::{Map, StoragePathEntry};
///
/// #[storage]
/// struct Storage {
///     balances: Map<ContractAddress, u256>,
/// }
///
/// // Get the storage path for the balance of a specific address
/// let balance_path = self.balances.entry(address);
/// ```
pub trait StoragePathEntry<C> {
    type Key;
    type Value;
    fn entry(self: C, key: Self::Key) -> StoragePath<Self::Value>;
}

/// A persistent key-value store in contract storage.
///
/// This type cannot be instantiated as it is marked with `#[phantom]`. This is by design:
/// `Map` is a compile-time type that only exists to provide type information for the compiler.
/// It represents a mapping in storage, but the actual storage operations are handled by the
/// [`StorageMapReadAccess`], [`StorageMapWriteAccess`], and [`StoragePathEntry`] traits.
#[phantom]
pub struct Map<K, V> {}

/// A trait for making a map like type support implement the `StoragePathEntry` trait.
trait EntryInfo<T> {
    type Key;
    type Value;
}

impl EntryInfoImpl<K, V> of EntryInfo<Map<K, V>> {
    type Key = K;
    type Value = V;
}

/// Implement `StoragePathEntry` for any `EntryInfo` type if their key implements `Hash`.
impl EntryInfoStoragePathEntry<
    T, +EntryInfo<T>, +core::hash::Hash<EntryInfo::<T>::Key, StoragePathHashState>,
> of StoragePathEntry<StoragePath<T>> {
    type Key = EntryInfo::<T>::Key;
    type Value = EntryInfo::<T>::Value;
    fn entry(self: StoragePath<T>, key: EntryInfo::<T>::Key) -> StoragePath<EntryInfo::<T>::Value> {
        self.update(key)
    }
}

/// Same as `StoragePathEntryMap`, but for `Mutable<T>`, forwards the Mutable wrapper onto the value
/// type.
impl MutableEntryStoragePathEntry<
    T,
    +MutableTrait<T>,
    impl EntryImpl: EntryInfo<MutableTrait::<T>::InnerType>,
    +core::hash::Hash<EntryImpl::Key, StoragePathHashState>,
> of StoragePathEntry<StoragePath<T>> {
    type Key = EntryImpl::Key;
    type Value = Mutable<EntryImpl::Value>;
    fn entry(self: StoragePath<T>, key: EntryImpl::Key) -> StoragePath<Mutable<EntryImpl::Value>> {
        self.update(key)
    }
}

/// Implement `StorageMapReadAccess` trait for any type that implements `StoragePathEntry` and
/// `Store`.
impl StorableEntryReadAccess<
    T,
    +EntryInfo<T>,
    +core::hash::Hash<EntryInfo::<T>::Key, StoragePathHashState>,
    +starknet::Store<EntryInfo::<T>::Value>,
> of StorageMapReadAccess<StoragePath<T>> {
    type Key = EntryInfo::<T>::Key;
    type Value = EntryInfo::<T>::Value;
    fn read(self: StoragePath<T>, key: EntryInfo::<T>::Key) -> EntryInfo::<T>::Value {
        self.entry(key).as_ptr().read()
    }
}

impl StorageAsPathReadForward<
    T,
    impl PathImpl: StorageAsPath<T>,
    impl AccessImpl: StorageMapReadAccess<StoragePath<PathImpl::Value>>,
    +Drop<T>,
    +Drop<AccessImpl::Key>,
> of StorageMapReadAccess<T> {
    type Key = AccessImpl::Key;
    type Value = AccessImpl::Value;
    #[inline]
    fn read(self: T, key: AccessImpl::Key) -> AccessImpl::Value {
        self.as_path().read(key)
    }
}

/// Implement `StorageMapReadAccess` trait for any mutable type that implements `StoragePathEntry`
/// and `Store`.
impl MutableStorableEntryReadAccess<
    T,
    +MutableTrait<T>,
    +EntryInfo<MutableTrait::<T>::InnerType>,
    +core::hash::Hash<EntryInfo::<MutableTrait::<T>::InnerType>::Key, StoragePathHashState>,
    +starknet::Store<EntryInfo::<MutableTrait::<T>::InnerType>::Value>,
> of StorageMapReadAccess<StoragePath<T>> {
    type Key = EntryInfo::<MutableTrait::<T>::InnerType>::Key;
    type Value = EntryInfo::<MutableTrait::<T>::InnerType>::Value;
    #[inline]
    fn read(
        self: StoragePath<T>, key: EntryInfo::<MutableTrait::<T>::InnerType>::Key,
    ) -> EntryInfo::<MutableTrait::<T>::InnerType>::Value {
        self.entry(key).as_ptr().read()
    }
}


/// Implement `StorageMapWriteAccess` trait for any mutable type that implements `StoragePathEntry`
/// and `Store`.
impl MutableStorableEntryWriteAccess<
    T,
    +MutableTrait<T>,
    +EntryInfo<MutableTrait::<T>::InnerType>,
    +core::hash::Hash<EntryInfo::<MutableTrait::<T>::InnerType>::Key, StoragePathHashState>,
    +starknet::Store<EntryInfo::<MutableTrait::<T>::InnerType>::Value>,
    +Drop<EntryInfo::<MutableTrait::<T>::InnerType>::Value>,
> of StorageMapWriteAccess<StoragePath<T>> {
    type Key = EntryInfo::<MutableTrait::<T>::InnerType>::Key;
    type Value = EntryInfo::<MutableTrait::<T>::InnerType>::Value;
    fn write(
        self: StoragePath<T>,
        key: EntryInfo::<MutableTrait::<T>::InnerType>::Key,
        value: EntryInfo::<MutableTrait::<T>::InnerType>::Value,
    ) {
        self.entry(key).as_ptr().write(value)
    }
}

impl StorageAsPathWriteForward<
    T,
    impl PathImpl: StorageAsPath<T>,
    impl AccessImpl: StorageMapWriteAccess<StoragePath<PathImpl::Value>>,
    +Drop<T>,
    +Drop<AccessImpl::Key>,
    +Drop<AccessImpl::Value>,
> of StorageMapWriteAccess<T> {
    type Key = AccessImpl::Key;
    type Value = AccessImpl::Value;
    fn write(self: T, key: AccessImpl::Key, value: AccessImpl::Value) {
        self.as_path().write(key, value)
    }
}

/// Implement `StoragePathEntry` for any type that implements `StoragePath` and `StoragePathEntry`.
impl PathableStorageEntryImpl<
    T,
    impl PathImpl: StorageAsPath<T>,
    impl EntryImpl: StoragePathEntry<StoragePath<PathImpl::Value>>,
    +Drop<T>,
    +Drop<EntryImpl::Key>,
> of StoragePathEntry<T> {
    type Key = EntryImpl::Key;
    type Value = EntryImpl::Value;
    fn entry(self: T, key: Self::Key) -> StoragePath<Self::Value> {
        let path = PathImpl::as_path(@self);
        EntryImpl::entry(path, key)
    }
}

/// Implementation of `ValidStorageTypeTrait` for `Map<key, value>`.
/// This ensures that Starknet storage maps (`Map<key, value>`) are valid storage types,
/// as long as:
/// - The `key` type implements `core::hash::Hash`, which allows it to be used in storage mappings.
/// - The `value` type implements `ValidStorageTypeTrait`, meaning it is itself a valid storage
/// type.
use starknet::storage::ValidStorageTypeTrait;
impl ValidStorageTypeTraitMapImpl<
    Key, Value, +core::hash::Hash<Key, StoragePathHashState>, +ValidStorageTypeTrait<Value>,
> of ValidStorageTypeTrait<Map<Key, Value>>;
