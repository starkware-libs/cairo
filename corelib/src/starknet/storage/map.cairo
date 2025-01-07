//! Key-value storage mapping implementation for Starknet contracts.
//!
//! This module provides the core mapping functionality used in Starknet smart contracts,
//! allowing contracts to store and retrieve values associated with keys in contract
//! storage.
//!
//! # Core Components
//!
//! * `Map<K,V>`: Main mapping type for key-value storage
//! * `StorageMapReadAccess`: Trait for reading values from mappings
//! * `StorageMapWriteAccess`: Trait for writing values to mappings
//! * `StoragePathEntry`: Trait for retrieving storage paths from keys
//!
//! # Examples
//!
//! ```
//! #[storage]
//! struct Storage {
//!     a: Map<felt252, felt52>,
//!     b: Map<felt52, Map<felt52, felt52>>,
//! }
//! ```

#[allow(unused_imports)]
use super::{
    Mutable, MutableTrait, StorageAsPath, StorageAsPointer, StoragePath, StoragePathHashState,
    StoragePathTrait, StoragePathUpdateTrait, StoragePointerReadAccess, StoragePointerWriteAccess,
};

/// Trait for reading a contract/component storage member in a specific key place.
///
/// # Examples
///
/// ```
/// #[storage]
/// struct Storage {
///     a: Map<felt252, felt52>,
///     b: Map<felt52, Map<felt52, felt52>>,
/// }
///
/// fn use_storage(self: @ContractState) {
///     let a_value = self.a.entry(1).read();
///     let b_value = self.b.entry(1).entry(2).read()
/// }
/// ```
pub trait StorageMapReadAccess<TMemberState> {
    type Key;
    type Value;
    fn read(self: TMemberState, key: Self::Key) -> Self::Value;
}

/// Trait for writing contract/component storage member in a specific key place.
///
/// # Examples
///
/// ```
/// #[storage]
/// struct Storage {
///     a: Map<felt252, felt52>,
///     b: Map<felt52, Map<felt52, felt52>>,
/// }
///
/// fn write_to_storage(self: ContractState) {
///     self.a.entry(1).write(0);
///     self.b.entry(1).entry(2).write(1)
/// }
/// ```
pub trait StorageMapWriteAccess<TMemberState> {
    type Key;
    type Value;
    fn write(self: TMemberState, key: Self::Key, value: Self::Value);
}

/// Trait for updating the hash state (storage path) with a value, using an `entry` method.
///
/// # Examples
///
/// ```
/// #[storage]
/// struct Storage {
///     a: Map<felt252, felt52>,
///     b: Map<felt52, Map<felt52, felt52>>,
/// }
///
/// let a_storage_path = self.a.entry(1);
/// ```
pub trait StoragePathEntry<C> {
    type Key;
    type Value;
    fn entry(self: C, key: Self::Key) -> StoragePath<Self::Value>;
}

/// A struct that represents a map in a contract storage.
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
