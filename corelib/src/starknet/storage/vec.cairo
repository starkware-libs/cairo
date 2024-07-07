use super::{
    StorageAsPath, StorageAsPointer, StoragePath, StoragePointer0Offset, Mutable, StoragePathTrait,
    StoragePathUpdateTrait, StoragePointerReadAccess, StoragePointerWriteAccess
};

/// A type to represent a vec in storage. The length of the storage is stored in the storage
/// base, while the elements are stored in hash(storage_base, index).
#[phantom]
pub struct Vec<T> {}

impl VecDrop<T> of Drop<Vec<T>> {}
impl VecCopy<T> of Copy<Vec<T>> {}

/// Implement as_ptr for Vec.
impl VecAsPointer<T> of StorageAsPointer<StoragePath<Vec<T>>> {
    type Value = u64;
    fn as_ptr(self: @StoragePath<Vec<T>>) -> StoragePointer0Offset<u64> {
        StoragePointer0Offset { address: (*self).finalize() }
    }
}

/// Implement as_ptr for Mutable<Vec>.
impl MutableVecAsPointer<T> of StorageAsPointer<StoragePath<Mutable<Vec<T>>>> {
    type Value = Mutable<u64>;
    fn as_ptr(self: @StoragePath<Mutable<Vec<T>>>) -> StoragePointer0Offset<Mutable<u64>> {
        StoragePointer0Offset { address: (*self).finalize() }
    }
}


/// Trait for the interface of a storage vec.
pub trait VecTrait<T> {
    type ElementType;
    fn at(self: T, index: u64) -> StoragePath<Self::ElementType>;
    fn len(self: T) -> u64;
}

/// Implement `VecTrait` for `StoragePath<Vec<T>>`.
impl VecImpl<T> of VecTrait<StoragePath<Vec<T>>> {
    type ElementType = T;
    fn at(self: StoragePath<Vec<T>>, index: u64) -> StoragePath<T> {
        let vec_len = self.len();
        assert!(index < vec_len, "Index out of bounds");
        self.update(index)
    }
    fn len(self: StoragePath<Vec<T>>) -> u64 {
        self.as_ptr().read()
    }
}

/// Implement `VecTrait` for any type that implements StorageAsPath into a storage path
/// that implements VecTrait.
impl PathableVecImpl<
    T,
    +Drop<T>,
    impl PathImpl: StorageAsPath<T>,
    impl VecTraitImpl: VecTrait<StoragePath<PathImpl::Value>>
> of VecTrait<T> {
    type ElementType = VecTraitImpl::ElementType;
    fn at(self: T, index: u64) -> StoragePath<VecTraitImpl::ElementType> {
        self.as_path().at(index)
    }
    fn len(self: T) -> u64 {
        self.as_path().len()
    }
}

/// Trait for the interface of a mutable storage vec.
pub trait MutableVecTrait<T> {
    type ElementType;
    fn at(self: T, index: u64) -> StoragePath<Mutable<Self::ElementType>>;
    fn len(self: T) -> u64;
    fn append(self: T) -> StoragePath<Mutable<Self::ElementType>>;
}

/// Implement `MutableVecTrait` for `StoragePath<Mutable<Vec<T>>`.
impl MutableVecImpl<T, +Drop<T>> of MutableVecTrait<StoragePath<Mutable<Vec<T>>>> {
    type ElementType = T;
    fn at(self: StoragePath<Mutable<Vec<T>>>, index: u64) -> StoragePath<Mutable<T>> {
        let vec_len = self.len();
        assert!(index < vec_len, "Index out of bounds");
        self.update(index)
    }
    fn len(self: StoragePath<Mutable<Vec<T>>>) -> u64 {
        self.as_ptr().read()
    }
    fn append(self: StoragePath<Mutable<Vec<T>>>) -> StoragePath<Mutable<T>> {
        let vec_len = self.len();
        self.as_ptr().write(vec_len + 1);
        self.update(vec_len)
    }
}

/// Implement `MutableVecTrait` for any type that implements StorageAsPath into a storage
/// path that implements MutableVecTrait.
impl PathableMutableVecImpl<
    T,
    +Drop<T>,
    impl PathImpl: StorageAsPath<T>,
    impl VecTraitImpl: MutableVecTrait<StoragePath<PathImpl::Value>>
> of MutableVecTrait<T> {
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

