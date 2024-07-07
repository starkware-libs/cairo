use super::{
    StorageAsPath, StorageAsPointer, StoragePath, StoragePointer0Offset, Mutable, StoragePathTrait,
    StoragePathUpdateTrait, StoragePointerReadAccess, StoragePointerWriteAccess
};

/// A type to represent a vec in storage. The length of the storage is stored in the storage
/// base, while the elements are stored in hash(storage_base, index).
#[phantom]
pub struct StorageVec<T> {}

impl StorageVecDrop<T> of Drop<StorageVec<T>> {}
impl StorageVecCopy<T> of Copy<StorageVec<T>> {}

/// Implement as_ptr for StorageVec.
impl StorageVecAsPointer<T> of StorageAsPointer<StoragePath<StorageVec<T>>> {
    type Value = u64;
    fn as_ptr(self: @StoragePath<StorageVec<T>>) -> StoragePointer0Offset<u64> {
        StoragePointer0Offset { address: (*self).finalize() }
    }
}

/// Implement as_ptr for Mutable<StorageVec>.
impl MutableStorageVecAsPointer<T> of StorageAsPointer<StoragePath<Mutable<StorageVec<T>>>> {
    type Value = Mutable<u64>;
    fn as_ptr(self: @StoragePath<Mutable<StorageVec<T>>>) -> StoragePointer0Offset<Mutable<u64>> {
        StoragePointer0Offset { address: (*self).finalize() }
    }
}


/// Trait for the interface of a storage vec.
pub trait StorageVecTrait<T> {
    type ElementType;
    fn at(self: T, index: u64) -> StoragePath<Self::ElementType>;
    fn len(self: T) -> u64;
}

/// Implement `StorageVecTrait` for `StoragePath<StorageVec<T>>`.
impl StorageVecImpl<T> of StorageVecTrait<StoragePath<StorageVec<T>>> {
    type ElementType = T;
    fn at(self: StoragePath<StorageVec<T>>, index: u64) -> StoragePath<T> {
        let vec_len = self.len();
        assert!(index < vec_len, "Index out of bounds");
        self.update(index)
    }
    fn len(self: StoragePath<StorageVec<T>>) -> u64 {
        self.as_ptr().read()
    }
}

/// Implement `StorageVecTrait` for any type that implements StorageAsPath into a storage path
/// that implements StorageVecTrait.
impl PathableStorageVecImpl<
    T,
    +Drop<T>,
    impl PathImpl: StorageAsPath<T>,
    impl VecTraitImpl: StorageVecTrait<StoragePath<PathImpl::Value>>
> of StorageVecTrait<T> {
    type ElementType = VecTraitImpl::ElementType;
    fn at(self: T, index: u64) -> StoragePath<VecTraitImpl::ElementType> {
        self.as_path().at(index)
    }
    fn len(self: T) -> u64 {
        self.as_path().len()
    }
}

/// Trait for the interface of a mutable storage vec.
pub trait MutableStorageVecTrait<T> {
    type ElementType;
    fn at(self: T, index: u64) -> StoragePath<Mutable<Self::ElementType>>;
    fn len(self: T) -> u64;
    fn append(self: T) -> StoragePath<Mutable<Self::ElementType>>;
}

/// Implement `MutableStorageVecTrait` for `StoragePath<Mutable<StorageVec<T>>`.
impl MutableStorageVecImpl<
    T, +Drop<T>
> of MutableStorageVecTrait<StoragePath<Mutable<StorageVec<T>>>> {
    type ElementType = T;
    fn at(self: StoragePath<Mutable<StorageVec<T>>>, index: u64) -> StoragePath<Mutable<T>> {
        let vec_len = self.len();
        assert!(index < vec_len, "Index out of bounds");
        self.update(index)
    }
    fn len(self: StoragePath<Mutable<StorageVec<T>>>) -> u64 {
        self.as_ptr().read()
    }
    fn append(self: StoragePath<Mutable<StorageVec<T>>>) -> StoragePath<Mutable<T>> {
        let vec_len = self.len();
        self.as_ptr().write(vec_len + 1);
        self.update(vec_len)
    }
}

/// Implement `MutableStorageVecTrait` for any type that implements StorageAsPath into a storage
/// path that implements MutableStorageVecTrait.
impl PathableMutableStorageVecImpl<
    T,
    +Drop<T>,
    impl PathImpl: StorageAsPath<T>,
    impl VecTraitImpl: MutableStorageVecTrait<StoragePath<PathImpl::Value>>
> of MutableStorageVecTrait<T> {
    type ElementType = VecTraitImpl::ElementType;
    fn at(self: T, index: u64) -> StoragePath<Mutable<VecTraitImpl::ElementType>> {
        self.as_path().at(index)
    }
    fn len(self: T) -> u64 {
        self.as_path().len()
    }
    fn append(self: T) -> StoragePath<Mutable<VecTraitImpl::ElementType>> {
        self.as_path().append()
    }
}

