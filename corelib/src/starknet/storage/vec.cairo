//! Vector-like storage collection for persisting data in contract storage.
//!
//! This module provides a vector-like collection that stores elements in contract storage.
//! Unlike memory arrays, storage vectors persist data onchain, meaning that values can be retrieved
//! even after the end of the current context.
//!
//! # Storage Layout
//!
//! A storage vector consists of two parts:
//! - The vector length stored at the base storage address (`sn_keccak(variable_name)`)
//! - The elements stored at addresses computed as `h(base_address, index)` where:
//!   - `h` is the Pedersen hash function
//!   - `index` is the element's position in the vector
//!
//! # Interacting with [`Vec`]
//!
//! Storage vectors can be accessed through two sets of traits:
//!
//! 1. Read-only access using `VecTrait`:
//!    ```
//!    // Get length
//!    let len = self.my_vec.len();
//!
//!    // Read element (panics if out of bounds)
//!    let value = self.my_vec.at(0).read();
//!
//!    // Read element (returns Option)
//!    let maybe_value: Option<u256> = self.my_vec.get(0).map(|ptr| ptr.read());
//!    ```
//!
//! 2. Mutable access using `MutableVecTrait`:
//!    ```
//!    // Append new element
//!    self.my_vec.append().write(value);
//!
//!    // Modify existing element
//!    self.my_vec.at(0).write(new_value);
//!    ```
//!
//! # Examples
//!
//! Basic usage:
//!
//! ```
//! use starknet::storage::{Vec, VecTrait, MutableVecTrait, StoragePointerReadAccess,
//! StoragePointerWriteAccess};
//!
//! #[storage]
//! struct Storage {
//!     numbers: Vec<u256>,
//! }
//!
//! fn store_number(ref self: ContractState, number: u256) {
//!     // Append new number
//!     self.numbers.append().write(number);
//!
//!     // Read first number
//!     let first = self.numbers[0].read();
//!
//!     // Get current length
//!     let size = self.numbers.len();
//! }
//! ```
//!
//! Loading the numbers into a memory array:
//!
//! ```
//! use starknet::storage::{Vec, VecTrait, StoragePointerReadAccess};
//!
//! fn to_array(self: @ContractState) -> Array<u256> {
//!     let mut arr = array![];
//!
//!     let len = self.numbers.len();
//!     for i in 0..len {
//!         arr.append(self.numbers[i].read());
//!     }
//!     arr
//! }
//! ```
use core::ops::Range;
use super::{
    IntoIterRange, Mutable, StorageAsPath, StorageAsPointer, StoragePath, StoragePathTrait,
    StoragePathUpdateTrait, StoragePointer0Offset, StoragePointerReadAccess,
    StoragePointerWriteAccess,
};

/// Represents a dynamic array in contract storage.
///
/// This type is zero-sized and cannot be instantiated.
/// Vectors can only be used in storage contexts and manipulated using the associated `VecTrait`
/// and `MutableVecTrait` traits.
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

/// Provides read-only access to elements in a storage [`Vec`].
///
/// This trait enables retrieving elements and checking the vector's length without
/// modifying the underlying storage.
pub trait VecTrait<T> {
    type ElementType;

    /// Returns a storage path to the element at the specified index, or `None` if out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use starknet::storage::{Vec, VecTrait, StoragePointerReadAccess};
    ///
    /// #[storage]
    /// struct Storage {
    ///     numbers: Vec<u256>,
    /// }
    ///
    /// fn maybe_number(self: @ContractState, index: u64) -> Option<u256> {
    ///     self.numbers.get(index).map(|ptr| ptr.read())
    /// }
    /// ```
    fn get(self: T, index: u64) -> Option<StoragePath<Self::ElementType>>;

    /// Returns a storage path to access the element at the specified index.
    ///
    /// # Panics
    ///
    /// Panics if the index is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use starknet::storage::{Vec, VecTrait, StoragePointerReadAccess};
    ///
    /// #[storage]
    /// struct Storage {
    ///     numbers: Vec<u256>,
    /// }
    ///
    /// fn get_number(self: @ContractState, index: u64) -> u256 {
    ///     self.numbers.at(index).read()
    /// }
    /// ```
    fn at(self: T, index: u64) -> StoragePath<Self::ElementType>;

    /// Returns the number of elements in the vector.
    ///
    /// The length is stored at the vector's base storage address and is automatically
    /// updated when elements are appended.
    ///
    /// # Examples
    ///
    /// ```
    /// use starknet::storage::{Vec, VecTrait};
    ///
    /// #[storage]
    /// struct Storage {
    ///     numbers: Vec<u256>,
    /// }
    ///
    /// fn is_empty(self: @ContractState) -> bool {
    ///     self.numbers.len() == 0
    /// }
    /// ```
    fn len(self: T) -> u64;
}

/// Implement `VecTrait` for `StoragePath<Vec<T>>`.
impl VecImpl<T> of VecTrait<StoragePath<Vec<T>>> {
    type ElementType = T;

    fn get(self: StoragePath<Vec<T>>, index: u64) -> Option<StoragePath<T>> {
        let vec_len = self.len();
        if index < vec_len {
            Some(self.update(index))
        } else {
            None
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

/// Provides mutable access to elements in a storage [`Vec`].
///
/// This trait extends the read functionality with methods to append new elements
/// and modify existing ones.
pub trait MutableVecTrait<T> {
    type ElementType;

    /// Returns a mutable storage path to the element at the specified index, or `None` if out of
    /// bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use starknet::storage::{Vec, MutableVecTrait, StoragePointerWriteAccess};
    ///
    /// #[storage]
    /// struct Storage {
    ///     numbers: Vec<u256>,
    /// }
    ///
    /// fn set_number(ref self: ContractState, index: u64, number: u256) -> bool {
    ///     if let Some(ptr) = self.numbers.get(index) {
    ///         ptr.write(number);
    ///         true
    ///     } else {
    ///         false
    ///     }
    /// }
    /// ```
    fn get(self: T, index: u64) -> Option<StoragePath<Mutable<Self::ElementType>>>;

    /// Returns a mutable storage path to the element at the specified index.
    ///
    /// # Panics
    ///
    /// Panics if the index is out of bounds.
    ///
    /// # Examples
    ///
    /// ```
    /// use starknet::storage::{Vec, MutableVecTrait, StoragePointerWriteAccess};
    ///
    /// #[storage]
    /// struct Storage {
    ///     numbers: Vec<u256>,
    /// }
    ///
    /// fn set_number(ref self: ContractState, index: u64, number: u256) {
    ///     self.numbers.at(index).write(number);
    /// }
    /// ```
    fn at(self: T, index: u64) -> StoragePath<Mutable<Self::ElementType>>;

    /// Returns the number of elements in the vector.
    ///
    /// The length is stored at the vector's base storage address and is automatically
    /// updated when elements are appended.
    ///
    /// # Examples
    ///
    /// ```
    /// use starknet::storage::{Vec, MutableVecTrait};
    ///
    /// #[storage]
    /// struct Storage {
    ///     numbers: Vec<u256>,
    /// }
    ///
    /// fn is_empty(self: @ContractState) -> bool {
    ///     self.numbers.len() == 0
    /// }
    /// ```
    fn len(self: T) -> u64;

    /// Returns a mutable storage path to write a new element at the end of the vector.
    ///
    /// This operation:
    /// 1. Increments the vector's length
    /// 2. Returns a storage path to write the new element
    ///
    /// # Examples
    ///
    /// ```
    /// use starknet::storage::{Vec, MutableVecTrait, StoragePointerWriteAccess};
    ///
    /// #[storage]
    /// struct Storage {
    ///     numbers: Vec<u256>,
    /// }
    ///
    /// fn push_number(ref self: ContractState, number: u256) {
    ///     self.numbers.append().write(number);
    /// }
    /// ```
    #[deprecated(
        feature: "starknet-storage-deprecation",
        note: "Use `starknet::storage::MutableVecTrait::push` instead.",
    )]
    fn append(self: T) -> StoragePath<Mutable<Self::ElementType>> {
        Self::allocate(self)
    }

    /// Allocates space for a new element at the end of the vector, returning a mutable storage path
    /// to write the element.
    ///
    /// This function is a replacement for the deprecated `append` function, which allowed
    /// appending new elements to a vector. Unlike `append`, `allocate` is specifically useful when
    /// you need to prepare space for elements of unknown or dynamic size (e.g., appending another
    /// vector).
    ///
    /// # Use Case
    ///
    /// `allocate` is essential when pushing a vector into another vector, as the size of the
    /// nested vector is unknown at compile time. It allows the caller to allocate the required
    /// space first, then write the nested vector into the allocated space using `.write()`.
    ///
    /// This is necessary because pushing directly (e.g., `vec.push(nested_vec)`) is not supported
    /// due to `Vec` being only a storage abstraction.
    ///
    /// # Deprecation Note
    ///
    /// The `append` function is now deprecated. Use `allocate` to achieve the same functionality
    /// with improved clarity and flexibility.
    ///
    /// # Examples
    ///
    /// ```
    /// use starknet::storage::{Vec, MutableVecTrait, StoragePointerWriteAccess};
    ///
    /// #[storage]
    /// struct Storage {
    ///     numbers: Vec<Vec<u256>>,
    /// }
    ///
    /// fn append_nested_vector(ref self: ContractState, elements: Array<u256>) {
    ///     // Allocate space for the nested vector in the outer vector.
    ///     let new_vec_storage_path = self.numbers.allocate();
    ///     for element in elements {
    ///         new_vec_storage_path.push(element)
    ///     }
    /// }
    /// ```
    fn allocate(self: T) -> StoragePath<Mutable<Self::ElementType>>;

    /// Pushes a new value onto the vector.
    ///
    /// This operation:
    /// 1. Increments the vector's length.
    /// 2. Writes the provided value to the new storage location at the end of the vector.
    ///
    /// # Note
    ///
    /// If you need to allocate storage without writing a value (e.g., when appending another
    /// vector), consider using [`allocate`] instead.
    ///
    /// # Examples
    ///
    /// ```
    /// use starknet::storage::{Vec, MutableVecTrait};
    ///
    /// #[storage]
    /// struct Storage {
    ///     numbers: Vec<u256>,
    /// }
    ///
    /// fn push_number(ref self: ContractState, number: u256) {
    ///     self.numbers.push(number);
    /// }
    /// ```
    fn push<+Drop<Self::ElementType>, +starknet::Store<Self::ElementType>>(
        self: T, value: Self::ElementType,
    );

    /// Pops the last value off the vector.
    ///
    /// This operation:
    /// 1. Retrieves the value stored at the last position in the vector.
    /// 2. Decrements the vector's length.
    /// 3. Returns the retrieved value or `None` if the vector is empty.
    ///
    /// # Examples
    ///
    /// ```
    /// use starknet::storage::{Vec, MutableVecTrait};
    ///
    /// #[storage]
    /// struct Storage {
    ///     numbers: Vec<u256>,
    /// }
    ///
    /// fn pop_number(ref self: ContractState) -> Option<u256> {
    ///     self.numbers.pop()
    /// }
    /// ```
    fn pop<+Drop<Self::ElementType>, +starknet::Store<Self::ElementType>>(
        self: T,
    ) -> Option<Self::ElementType>;
}

/// Implement `MutableVecTrait` for `StoragePath<Mutable<Vec<T>>`.
impl MutableVecImpl<T> of MutableVecTrait<StoragePath<Mutable<Vec<T>>>> {
    type ElementType = T;

    fn get(self: StoragePath<Mutable<Vec<T>>>, index: u64) -> Option<StoragePath<Mutable<T>>> {
        let vec_len = self.len();
        if index < vec_len {
            Some(self.update(index))
        } else {
            None
        }
    }

    fn at(self: StoragePath<Mutable<Vec<T>>>, index: u64) -> StoragePath<Mutable<T>> {
        assert!(index < self.len(), "Index out of bounds");
        self.update(index)
    }

    fn len(self: StoragePath<Mutable<Vec<T>>>) -> u64 {
        self.as_ptr().read()
    }

    fn allocate(self: StoragePath<Mutable<Vec<T>>>) -> StoragePath<Mutable<T>> {
        let vec_len = self.len();
        self.as_ptr().write(vec_len + 1);
        self.update(vec_len)
    }

    fn push<+Drop<Self::ElementType>, +starknet::Store<Self::ElementType>>(
        self: StoragePath<Mutable<Vec<T>>>, value: Self::ElementType,
    ) {
        self.allocate().write(value);
    }

    fn pop<+Drop<Self::ElementType>, +starknet::Store<Self::ElementType>>(
        self: StoragePath<Mutable<Vec<T>>>,
    ) -> Option<Self::ElementType> {
        let len_ptr = self.as_ptr();
        let vec_len: u64 = len_ptr.read();
        if vec_len == 0 {
            return None;
        }
        let entry: StoragePath<Mutable<T>> = self.update(vec_len - 1);
        let last_element = entry.read();
        // Remove the element's data from the storage.
        let entry_ptr = entry.as_ptr();
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<
                Self::ElementType,
            >::scrub(0, entry_ptr.__storage_pointer_address__, 0),
        );
        len_ptr.write(vec_len - 1);
        Some(last_element)
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

    fn allocate(self: T) -> StoragePath<Mutable<VecTraitImpl::ElementType>> {
        self.as_path().allocate()
    }

    fn push<+Drop<Self::ElementType>, +starknet::Store<Self::ElementType>>(
        self: T, value: Self::ElementType,
    ) {
        self.as_path().push(value)
    }

    fn pop<+Drop<Self::ElementType>, +starknet::Store<Self::ElementType>>(
        self: T,
    ) -> Option<Self::ElementType> {
        self.as_path().pop()
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

/// An iterator struct over a `Vec` in storage.
#[derive(Drop)]
pub struct VecIter<T, impl VecTraitImpl: VecTrait<T>> {
    vec: T,
    current_index: crate::ops::RangeIterator<u64>,
}

impl VecIterator<T, impl VecTraitImpl: VecTrait<T>, +Drop<T>, +Copy<T>> of Iterator<VecIter<T>> {
    type Item = StoragePath<VecTraitImpl::ElementType>;
    fn next(ref self: VecIter<T>) -> Option<Self::Item> {
        self.vec.get(self.current_index.next()?)
    }
}

// Implement `IntoIterRange` for `StoragePath<Vec<T>>`
pub impl VecIntoIterRange<
    T, impl VecTraitImpl: VecTrait<StoragePath<Vec<T>>>,
> of IntoIterRange<StoragePath<Vec<T>>> {
    type IntoIter = VecIter<StoragePath<Vec<T>>, VecTraitImpl>;
    #[inline]
    fn into_iter_range(self: StoragePath<Vec<T>>, range: Range<u64>) -> Self::IntoIter {
        VecIter { current_index: range.into_iter(), vec: self }
    }
    #[inline]
    fn into_iter_full_range(self: StoragePath<Vec<T>>) -> Self::IntoIter {
        VecIter { current_index: (0..core::num::traits::Bounded::MAX).into_iter(), vec: self }
    }
}

/// Implement `IntoIterRange` for any type that implements StorageAsPath into a storage path
/// that implements VecTrait.
pub impl PathableVecIntoIterRange<
    T,
    +Destruct<T>,
    impl PathImpl: StorageAsPath<T>,
    impl VecTraitImpl: VecTrait<StoragePath<PathImpl::Value>>,
> of IntoIterRange<T> {
    type IntoIter = VecIter<StoragePath<PathImpl::Value>, VecTraitImpl>;
    #[inline]
    fn into_iter_range(self: T, range: Range<u64>) -> Self::IntoIter {
        VecIter { current_index: range.into_iter(), vec: self.as_path() }
    }
    #[inline]
    fn into_iter_full_range(self: T) -> Self::IntoIter {
        let vec = self.as_path();
        VecIter { current_index: (0..core::num::traits::Bounded::MAX).into_iter(), vec }
    }
}

/// An iterator struct over a `Mutable<Vec>` in storage.
#[derive(Drop)]
struct MutableVecIter<T, impl MutVecTraitImpl: MutableVecTrait<T>> {
    vec: T,
    current_index: crate::ops::RangeIterator<u64>,
}

impl MutableVecIterator<
    T, +Drop<T>, +Copy<T>, impl MutVecTraitImpl: MutableVecTrait<T>,
> of Iterator<MutableVecIter<T>> {
    type Item = StoragePath<Mutable<MutVecTraitImpl::ElementType>>;
    fn next(ref self: MutableVecIter<T>) -> Option<Self::Item> {
        self.vec.get(self.current_index.next()?)
    }
}

// Implement `IntoIterRange` for `StoragePath<Mutable<Vec<T>>>`
pub impl MutableVecIntoIterRange<
    T, impl MutVecTraitImpl: MutableVecTrait<StoragePath<Mutable<Vec<T>>>>,
> of IntoIterRange<StoragePath<Mutable<Vec<T>>>> {
    type IntoIter = MutableVecIter<StoragePath<Mutable<Vec<T>>>, MutVecTraitImpl>;
    #[inline]
    fn into_iter_range(self: StoragePath<Mutable<Vec<T>>>, range: Range<u64>) -> Self::IntoIter {
        MutableVecIter { current_index: range.into_iter(), vec: self }
    }
    #[inline]
    fn into_iter_full_range(self: StoragePath<Mutable<Vec<T>>>) -> Self::IntoIter {
        MutableVecIter {
            current_index: (0..core::num::traits::Bounded::MAX).into_iter(), vec: self,
        }
    }
}

/// Implement `IntoIterRange` for any type that implements StorageAsPath into a storage path
/// that implements MutableVecTrait.
pub impl PathableMutableVecIntoIterRange<
    T,
    +Destruct<T>,
    impl PathImpl: StorageAsPath<T>,
    impl MutVecTraitImpl: MutableVecTrait<StoragePath<PathImpl::Value>>,
> of IntoIterRange<T> {
    type IntoIter = MutableVecIter<StoragePath<PathImpl::Value>, MutVecTraitImpl>;
    #[inline]
    fn into_iter_range(self: T, range: Range<u64>) -> Self::IntoIter {
        MutableVecIter { current_index: range.into_iter(), vec: self.as_path() }
    }
    #[inline]
    fn into_iter_full_range(self: T) -> Self::IntoIter {
        let vec = self.as_path();
        MutableVecIter { current_index: (0..core::num::traits::Bounded::MAX).into_iter(), vec }
    }
}

/// Implementation of `ValidStorageTypeTrait` for `Vec<T>`.
/// This ensures that Starknet storage vectors (`Vec<T>`) are valid storage types,
/// as long as the contained type `T` is itself a valid storage type (`ValidStorageTypeTrait<T>`).
/// This allows vectors to be stored in contract storage, provided that their elements
/// conform to the required storage constraints.
use starknet::storage::ValidStorageTypeTrait;
impl ValidStorageTypeVecTrait<T, +ValidStorageTypeTrait<T>> of ValidStorageTypeTrait<Vec<T>>;
