use core::Option;
use super::{
    Mutable, StorageAsPath, StorageAsPointer, StoragePath, StoragePathTrait, StoragePathUpdateTrait,
    StoragePointer0Offset, StoragePointerReadAccess, StoragePointerWriteAccess,
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
        StoragePointer0Offset { __storage_pointer_address__: (*self).finalize() }
    }
}

/// Implement as_ptr for Mutable<Vec>.
impl MutableVecAsPointer<T> of StorageAsPointer<StoragePath<Mutable<Vec<T>>>> {
    type Value = Mutable<u64>;
    fn as_ptr(self: @StoragePath<Mutable<Vec<T>>>) -> StoragePointer0Offset<Mutable<u64>> {
        StoragePointer0Offset { __storage_pointer_address__: (*self).finalize() }
    }
}

/// Trait for the interface of a storage vec.
pub trait VecTrait<T> {
    type ElementType;
    fn get(self: T, index: u64) -> Option<StoragePath<Self::ElementType>>;
    fn at(self: T, index: u64) -> StoragePath<Self::ElementType>;
    fn len(self: T) -> u64;
}

/// Implement `VecTrait` for `StoragePath<Vec<T>>`.
impl VecImpl<T> of VecTrait<StoragePath<Vec<T>>> {
    type ElementType = T;
    fn get(self: StoragePath<Vec<T>>, index: u64) -> Option<StoragePath<T>> {
        let vec_len = self.len();
        if index < vec_len {
            Option::Some(self.update(index))
        } else {
            Option::None
        }
    }
    fn at(self: StoragePath<Vec<T>>, index: u64) -> StoragePath<T> {
        assert!(index < self.len(), "Index out of bounds");
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
    impl VecTraitImpl: VecTrait<StoragePath<PathImpl::Value>>,
> of VecTrait<T> {
    type ElementType = VecTraitImpl::ElementType;
    fn get(self: T, index: u64) -> Option<StoragePath<VecTraitImpl::ElementType>> {
        self.as_path().get(index)
    }
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
    fn get(self: T, index: u64) -> Option<StoragePath<Mutable<Self::ElementType>>>;
    fn at(self: T, index: u64) -> StoragePath<Mutable<Self::ElementType>>;
    fn len(self: T) -> u64;
    fn append(self: T) -> StoragePath<Mutable<Self::ElementType>>;
}

/// Implement `MutableVecTrait` for `StoragePath<Mutable<Vec<T>>`.
impl MutableVecImpl<T> of MutableVecTrait<StoragePath<Mutable<Vec<T>>>> {
    type ElementType = T;
    fn get(self: StoragePath<Mutable<Vec<T>>>, index: u64) -> Option<StoragePath<Mutable<T>>> {
        let vec_len = self.len();
        if index < vec_len {
            Option::Some(self.update(index))
        } else {
            Option::None
        }
    }
    fn at(self: StoragePath<Mutable<Vec<T>>>, index: u64) -> StoragePath<Mutable<T>> {
        assert!(index < self.len(), "Index out of bounds");
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
    impl VecTraitImpl: MutableVecTrait<StoragePath<PathImpl::Value>>,
> of MutableVecTrait<T> {
    type ElementType = VecTraitImpl::ElementType;
    fn get(self: T, index: u64) -> Option<StoragePath<Mutable<VecTraitImpl::ElementType>>> {
        self.as_path().get(index)
    }
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

pub impl VecIndexView<
    VecT, impl VecImpl: VecTrait<VecT>, +Copy<VecT>,
> of core::ops::IndexView<VecT, u64> {
    type Target = StoragePath<VecImpl::ElementType>;
    fn index(self: @VecT, index: u64) -> Self::Target {
        (*self).at(index)
    }
}

pub impl MutableVecIndexView<
    VecT, impl VecImpl: MutableVecTrait<VecT>, +Copy<VecT>,
> of core::ops::IndexView<VecT, u64> {
    type Target = StoragePath<Mutable<VecImpl::ElementType>>;
    fn index(self: @VecT, index: u64) -> Self::Target {
        (*self).at(index)
    }
}

/// An iterator struct over a Vec in storage.
#[derive(Drop)]
pub struct VecIter<T, impl VecImpl: VecTrait<T>> {
    vec: T,
    alive: crate::ops::range::RangeIterator<u64>,
}

impl VecIterator<
    T, impl VecImpl: VecTrait<T>, +Drop<T>, +Copy<T>,
> of core::iter::Iterator<VecIter<T>> {
    type Item = StoragePath<VecImpl::ElementType>;
    fn next(ref self: VecIter<T>) -> Option<Self::Item> {
        self.vec.get(self.alive.next()?)
    }
}

pub impl VecIntoIterator<
    T, impl VecImpl: VecTrait<T>, +Drop<T>, +Copy<T>,
> of crate::iter::IntoIterator<T> {
    type IntoIter = VecIter<T, VecImpl>;
    #[inline]
    fn into_iter(self: T) -> Self::IntoIter {
        VecIter { alive: (0..self.len()).into_iter(), vec: self }
    }
}

pub impl PathableVecIntoIterator<
    T,
    +Drop<T>,
    +Copy<T>,
    impl PathImpl: StorageAsPath<T>,
    impl VecImpl: VecTrait<StoragePath<PathImpl::Value>>,
> of crate::iter::IntoIterator<T> {
    type IntoIter = VecIter<StoragePath<PathImpl::Value>, VecImpl>;
    #[inline]
    fn into_iter(self: T) -> Self::IntoIter {
        VecIter { alive: (0..self.len()).into_iter(), vec: self.as_path() }
    }
}

/// An iterator struct over a MutableVec in storage.
#[derive(Drop)]
pub struct MutableVecIter<T, impl MutVecImpl: MutableVecTrait<T>> {
    vec: T,
    alive: crate::ops::range::RangeIterator<u64>,
}

impl MutableVecIterator<
    T, +Drop<T>, +Copy<T>, impl MutVecImpl: MutableVecTrait<T>,
> of core::iter::Iterator<MutableVecIter<T>> {
    type Item = StoragePath<Mutable<MutVecImpl::ElementType>>;
    fn next(ref self: MutableVecIter<T>) -> Option<Self::Item> {
        self.vec.get(self.alive.next()?)
    }
}

pub impl MutableVecIntoIterator<
    T, +Drop<T>, +Copy<T>, impl MutVecImpl: MutableVecTrait<T>,
> of crate::iter::IntoIterator<T> {
    type IntoIter = MutableVecIter<T, MutVecImpl>;
    #[inline]
    fn into_iter(self: T) -> Self::IntoIter {
        MutableVecIter { alive: (0..self.len()).into_iter(), vec: self }
    }
}

pub impl PathableMutableVecIntoIterator<
    T,
    +Drop<T>,
    +Copy<T>,
    impl PathImpl: StorageAsPath<T>,
    impl MutVecImpl: MutableVecTrait<StoragePath<PathImpl::Value>>,
> of crate::iter::IntoIterator<T> {
    type IntoIter = MutableVecIter<StoragePath<PathImpl::Value>, MutVecImpl>;
    #[inline]
    fn into_iter(self: T) -> Self::IntoIter {
        MutableVecIter { alive: (0..self.len()).into_iter(), vec: self.as_path() }
    }
}
