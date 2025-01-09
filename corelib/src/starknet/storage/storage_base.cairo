//! Core abstractions for contract storage management.
//!
//! This module provides the types and traits for handling contract storage internally
//! within the Cairo core library. Most developers should not need to implement these traits
//! directly, as they are primarily used by the storage system implementation.
//!
//! If you're writing a regular Starknet contract, you should use the high-level storage
//! traits and types, interacting with the members of the storage struct directly.

use super::{Mutable, StorageAsPath, StoragePath, StoragePathTrait};

/// A struct for holding an address to initialize a storage path with. The members (not direct
/// members, but accessible using `deref`) of a contract state are either `StorageBase` or
/// `FlattenedStorage` instances, with the generic type representing the type of the stored member.
pub struct StorageBase<T> {
    pub __base_address__: felt252,
}

impl StorageBaseDrop<T> of Drop<StorageBase<T>> {}
impl StorageBaseCopy<T> of Copy<StorageBase<T>> {}

impl StorageBaseAsPath<T> of StorageAsPath<StorageBase<T>> {
    type Value = T;
    fn as_path(self: @StorageBase<T>) -> StoragePath<T> {
        StoragePathTrait::new(*self.__base_address__)
    }
}

impl StorageBaseDeref<T> of core::ops::Deref<StorageBase<T>> {
    type Target = StoragePath<T>;
    fn deref(self: StorageBase<T>) -> Self::Target {
        self.as_path()
    }
}

/// A type that represents a flattened storage, i.e. a storage object which does not have any effect
/// on the path taken into consideration when computing the address of the storage object.
pub struct FlattenedStorage<T> {}

impl FlattenedStorageDrop<T> of Drop<FlattenedStorage<T>> {}
impl FlattenedStorageCopy<T> of Copy<FlattenedStorage<T>> {}

/// Dereference a flattened storage into a the storage object containing the members of the object.
impl FlattenedStorageDeref<
    T, impl StorageImpl: StorageTrait<T>,
> of core::ops::Deref<FlattenedStorage<T>> {
    type Target = StorageImpl::BaseType;
    fn deref(self: FlattenedStorage<T>) -> Self::Target {
        self.storage()
    }
}

/// Dereference a mutable flattened storage into a the storage object containing a mutable version
/// of the members of the object.
impl MutableFlattenedStorageDeref<
    T, impl StorageImpl: StorageTraitMut<T>,
> of core::ops::Deref<FlattenedStorage<Mutable<T>>> {
    type Target = StorageImpl::BaseType;
    fn deref(self: FlattenedStorage<Mutable<T>>) -> Self::Target {
        self.storage_mut()
    }
}

/// A trait for creating the struct containing the `StorageBase` or `FlattenedStorage` of all the
/// members of a contract state.
pub trait StorageTrait<T> {
    /// The type of the struct containing the `StorageBase` or `FlattenedStorage` of all the members
    /// of the type `T`.
    type BaseType;
    /// Creates a struct containing the `StorageBase` or `FlattenedStorage` of all the members of a
    /// contract state. Should be called from the `deref` method of the contract state.
    fn storage(self: FlattenedStorage<T>) -> Self::BaseType;
}

/// A trait for creating the struct containing the mutable `StorageBase` or `FlattenedStorage` of
/// all the members of a contract state.
pub trait StorageTraitMut<T> {
    /// The type of the struct containing the mutable `StorageBase` or `FlattenedStorage` of all the
    /// members of the type `T`.
    type BaseType;
    /// Creates a struct containing a mutable version of the `StorageBase` or `FlattenedStorage` of
    /// all the members of a contract state. Should be called from the `deref` method of the
    /// contract state.
    fn storage_mut(self: FlattenedStorage<Mutable<T>>) -> Self::BaseType;
}
