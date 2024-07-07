use super::{
    StorageAsPath, StorageAsPointer, StoragePath, StoragePointer0Offset, Mutable, StoragePathTrait,
    StoragePathUpdateTrait, StoragePointerReadAccess, StoragePointerWriteAccess
};

/// A type to represent an array in storage. The length of the storage is stored in the storage
/// base, while the elements are stored in hash(storage_base, index).
#[phantom]
pub struct StorageArray<T> {}

impl StorageArrayDrop<T> of Drop<StorageArray<T>> {}
impl StorageArrayCopy<T> of Copy<StorageArray<T>> {}

/// Implement as_ptr for StorageArray.
impl StorageArrayAsPointer<T> of StorageAsPointer<StoragePath<StorageArray<T>>> {
    type Value = u64;
    fn as_ptr(self: @StoragePath<StorageArray<T>>) -> StoragePointer0Offset<u64> {
        StoragePointer0Offset { address: (*self).finalize() }
    }
}

/// Implement as_ptr for Mutable<StorageArray>.
impl MutableStorageArrayAsPointer<T> of StorageAsPointer<StoragePath<Mutable<StorageArray<T>>>> {
    type Value = Mutable<u64>;
    fn as_ptr(self: @StoragePath<Mutable<StorageArray<T>>>) -> StoragePointer0Offset<Mutable<u64>> {
        StoragePointer0Offset { address: (*self).finalize() }
    }
}


/// Trait for the interface of a storage array.
pub trait StorageArrayTrait<T> {
    type ElementType;
    fn at(self: T, index: u64) -> StoragePath<Self::ElementType>;
    fn len(self: T) -> u64;
}

/// Implement `StorageArrayTrait` for `StoragePath<StorageArray<T>>`.
impl StorageArrayImpl<T> of StorageArrayTrait<StoragePath<StorageArray<T>>> {
    type ElementType = T;
    fn at(self: StoragePath<StorageArray<T>>, index: u64) -> StoragePath<T> {
        let array_len = self.len();
        assert!(index < array_len, "Index out of bounds");
        self.update(index)
    }
    fn len(self: StoragePath<StorageArray<T>>) -> u64 {
        self.as_ptr().read()
    }
}

/// Implement `StorageArrayTrait` for any type that implements StorageAsPath into a storage path
/// that implements StorageArrayTrait.
impl PathableStorageArrayImpl<
    T,
    +Drop<T>,
    impl PathImpl: StorageAsPath<T>,
    impl ArrayTraitImpl: StorageArrayTrait<StoragePath<PathImpl::Value>>
> of StorageArrayTrait<T> {
    type ElementType = ArrayTraitImpl::ElementType;
    fn at(self: T, index: u64) -> StoragePath<ArrayTraitImpl::ElementType> {
        self.as_path().at(index)
    }
    fn len(self: T) -> u64 {
        self.as_path().len()
    }
}

/// Trait for the interface of a mutable storage array.
pub trait MutableStorageArrayTrait<T> {
    type ElementType;
    fn at(self: T, index: u64) -> StoragePath<Mutable<Self::ElementType>>;
    fn len(self: T) -> u64;
    fn append(self: T) -> StoragePath<Mutable<Self::ElementType>>;
}

/// Implement `MutableStorageArrayTrait` for `StoragePath<Mutable<StorageArray<T>>`.
impl MutableStorageArrayImpl<
    T, +Drop<T>
> of MutableStorageArrayTrait<StoragePath<Mutable<StorageArray<T>>>> {
    type ElementType = T;
    fn at(self: StoragePath<Mutable<StorageArray<T>>>, index: u64) -> StoragePath<Mutable<T>> {
        let array_len = self.len();
        assert!(index < array_len, "Index out of bounds");
        self.update(index)
    }
    fn len(self: StoragePath<Mutable<StorageArray<T>>>) -> u64 {
        self.as_ptr().read()
    }
    fn append(self: StoragePath<Mutable<StorageArray<T>>>) -> StoragePath<Mutable<T>> {
        let array_len = self.len();
        self.as_ptr().write(array_len + 1);
        self.update(array_len)
    }
}

/// Implement `MutableStorageArrayTrait` for any type that implements StorageAsPath into a storage
/// path that implements MutableStorageArrayTrait.
impl PathableMutableStorageArrayImpl<
    T,
    +Drop<T>,
    impl PathImpl: StorageAsPath<T>,
    impl ArrayTraitImpl: MutableStorageArrayTrait<StoragePath<PathImpl::Value>>
> of MutableStorageArrayTrait<T> {
    type ElementType = ArrayTraitImpl::ElementType;
    fn at(self: T, index: u64) -> StoragePath<Mutable<ArrayTraitImpl::ElementType>> {
        self.as_path().at(index)
    }
    fn len(self: T) -> u64 {
        self.as_path().len()
    }
    fn append(self: T) -> StoragePath<Mutable<ArrayTraitImpl::ElementType>> {
        self.as_path().append()
    }
}

