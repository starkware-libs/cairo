//! Storage vector implementation for contracts.
//!
//! This module provides a dynamic array implementation that stores elements in contract storage
//! where:
//! - Vector length is stored at the base storage address
//! - Elements are stored at hash(base_address, index)
//! - Supports both mutable and immutable access patterns
//!
//! Two main traits define vector behavior:
//! - `VecTrait`: Read-only operations (get, at, len)
//! - `MutableVecTrait`: Adds write operations (append)
//!
//! # Examples
//!
//! ```
//! #[storage]
//! struct Storage {
//!     a: Vec<felt252>,
//! }
//!
//! self.a.append().write(0);
//! let len = self.a.len();
//! let element = self.a.at(0).read();
//! ```

use core::Option;
use super::{
    Mutable, StorageAsPath, StorageAsPointer, StoragePath, StoragePathTrait, StoragePathUpdateTrait,
    StoragePointer0Offset, StoragePointerReadAccess, StoragePointerWriteAccess,
};

/// A type to represent a vector in storage. The length of the storage is stored in the storage
/// base, while the elements are stored at hash(storage_base, index).
#[phantom]
pub struct Vec<T> {}

impl VecDrop<T> of Drop<Vec<T>> {}
impl VecCopy<T> of Copy<Vec<T>> {}

/// Implement `as_ptr` for `Vec`.
impl VecAsPointer<T> of StorageAsPointer<StoragePath<Vec<T>>> {
    type Value = u64;
    fn as_ptr(self: @StoragePath<Vec<T>>) -> StoragePointer0Offset<u64> {
        StoragePointer0Offset { __storage_pointer_address__: (*self).finalize() }
    }
}

/// Implement `as_ptr` for `Mutable<Vec>`.
impl MutableVecAsPointer<T> of StorageAsPointer<StoragePath<Mutable<Vec<T>>>> {
    type Value = Mutable<u64>;
    fn as_ptr(self: @StoragePath<Mutable<Vec<T>>>) -> StoragePointer0Offset<Mutable<u64>> {
        StoragePointer0Offset { __storage_pointer_address__: (*self).finalize() }
    }
}

/// Trait for the interface of a storage vec.
pub trait VecTrait<T> {
    type ElementType;

    /// Returns an option of a `StoragePath` which is then used to retrieve the element stored at
    /// the given `index` with `read` method.
    ///
    /// # Examples
    ///
    /// ```
    /// #[storage]
    /// struct Storage {
    ///     a: Vec<felt252>,
    /// }
    ///
    /// let element = self.a.get(0).unwrap_or(0).read();
    /// ```
    fn get(self: T, index: u64) -> Option<StoragePath<Self::ElementType>>;

    /// Returns a `StoragePath` which is then used to retrieve the element stored at the given
    /// `index` with `read` method.
    ///
    /// # Examples
    ///
    /// ```
    /// #[storage]
    /// struct Storage {
    ///     a: Vec<felt252>,
    /// }
    ///
    /// let element = self.a.at(0).read();
    /// ```
    fn at(self: T, index: u64) -> StoragePath<Self::ElementType>;

    /// Returns the length of the vec.
    ///
    /// # Examples
    ///
    /// ```
    /// #[storage]
    /// struct Storage {
    ///     a: Vec<felt252>,
    /// }
    ///
    /// let len = self.a.len;
    /// ```
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

/// Implement `VecTrait` for any type that implements `StorageAsPath` into a storage path
/// that implements `VecTrait`.
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

    /// Returns an option of a `StoragePath` which is then used to retrieve the element stored at
    /// the given `index` with `read` method.
    ///
    /// # Examples
    ///
    /// ```
    /// #[storage]
    /// struct Storage {
    ///     a: Vec<felt252>,
    /// }
    ///
    /// let element = self.a.get(0).unwrap_or(0).read();
    /// ```
    fn get(self: T, index: u64) -> Option<StoragePath<Mutable<Self::ElementType>>>;

    /// Returns a `StoragePath` which is then used to retrieve the element stored at the given
    /// `index` with `read` method.
    ///
    /// # Examples
    ///
    /// ```
    /// #[storage]
    /// struct Storage {
    ///     a: Vec<felt252>,
    /// }
    ///
    /// let element = self.a.at(0);
    /// ```
    fn at(self: T, index: u64) -> StoragePath<Mutable<Self::ElementType>>;

    /// Returns the length of the vec.
    ///
    /// # Examples
    ///
    /// ```
    /// #[storage]
    /// struct Storage {
    ///     a: Vec<felt252>,
    /// }
    ///
    /// let len = self.a.len;
    /// ```
    fn len(self: T) -> u64;

    /// Returns a `StoragePath` which is then used to append a value to the vec with `write`.
    ///
    /// # Examples
    ///
    /// ```
    /// #[storage]
    /// struct Storage {
    ///     a: Vec<felt252>,
    /// }
    ///
    /// self.a.append().write(0);
    /// ```
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
