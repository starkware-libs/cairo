use super::{Mutable, StorageAsPointer, StoragePointer, StoragePointer0Offset};

/// Similar to storage node, but for structs which are stored sequentially in the storage. In
/// contrast to storage node, the fields of the struct are just at an offset from the base address
/// of the struct.
pub trait SubPointers<T> {
    /// The type of the storage pointers, generated for the struct T.
    type SubPointersType;
    /// Creates a sub pointers struct for the given storage pointer to a struct T.
    fn sub_pointers(self: StoragePointer<T>) -> Self::SubPointersType;
}

/// A mutable version of `SubPointers`, works the same way, but on `Mutable<T>`.
pub trait SubPointersMut<T> {
    /// The type of the storage pointers, generated for the struct T.
    type SubPointersType;
    /// Creates a sub pointers struct for the given storage pointer to a struct T.
    fn sub_pointers_mut(self: StoragePointer<Mutable<T>>) -> Self::SubPointersType;
}

/// A trait for implementing `SubPointers` for types which are not a `StoragePointer`, such as
/// `StorageBase` and `StoragePath`.
pub trait SubPointersForward<T> {
    type SubPointersType;
    fn sub_pointers(self: T) -> Self::SubPointersType;
}

/// Implementation of SubPointersForward for `StoragePointer0Offset`.
impl Pointer0OffsetSubPointersForward<
    T, impl SubPointersImpl: SubPointers<T>,
> of SubPointersForward<StoragePointer0Offset<T>> {
    type SubPointersType = SubPointersImpl::SubPointersType;
    fn sub_pointers(self: StoragePointer0Offset<T>) -> SubPointersImpl::SubPointersType {
        self.deref().sub_pointers()
    }
}

/// Implementation of SubPointersForward for types which supports `StorageAsPointer`, i.e. being
/// able to convert to a `StoragePointer0Offset`, and the resulting type should implement
/// `SubPointersForward` (see `Pointer0OffsetSubPointersForward`).
impl SubPointersForwardImpl<
    T,
    +Drop<T>,
    impl AsPointerImpl: StorageAsPointer<T>,
    impl PointerForwardImpl: SubPointersForward<StoragePointer0Offset<AsPointerImpl::Value>>,
> of SubPointersForward<T> {
    type SubPointersType = PointerForwardImpl::SubPointersType;
    fn sub_pointers(self: T) -> Self::SubPointersType {
        self.as_ptr().sub_pointers()
    }
}

/// A trait for implementing `SubPointersMut` for types which are not a `StoragePointer`, such as
/// `StorageBase` and `StoragePath`.
pub trait SubPointersMutForward<T> {
    type SubPointersType;
    fn sub_pointers_mut(self: T) -> Self::SubPointersType;
}

/// Implementation of SubPointersMutForward for `StoragePointer0Offset`.
impl Pointer0OffsetSubPointersMutForward<
    T, impl SubPointersImpl: SubPointersMut<T>,
> of SubPointersMutForward<StoragePointer0Offset<Mutable<T>>> {
    type SubPointersType = SubPointersImpl::SubPointersType;
    fn sub_pointers_mut(
        self: StoragePointer0Offset<Mutable<T>>,
    ) -> SubPointersImpl::SubPointersType {
        self.deref().sub_pointers_mut()
    }
}

/// Implementation of SubPointersMutForward for types which supports `StorageAsPointer`, i.e. being
/// able to convert to a `StoragePointer0Offset`, and the resulting type should implement
/// `SubPointersMutForward` (see `Pointer0OffsetSubPointersMutForward`).
impl SubPointersMutForwardImpl<
    T,
    +Drop<T>,
    impl AsPointerImpl: StorageAsPointer<T>,
    impl PointerForwardImpl: SubPointersMutForward<StoragePointer0Offset<AsPointerImpl::Value>>,
> of SubPointersMutForward<T> {
    type SubPointersType = PointerForwardImpl::SubPointersType;
    fn sub_pointers_mut(self: T) -> Self::SubPointersType {
        self.as_ptr().sub_pointers_mut()
    }
}

/// Implementation of SubPointers for core types.
#[derive(Drop, Copy)]
struct u256SubPointers {
    pub low: starknet::storage::StoragePointer<u128>,
    pub high: starknet::storage::StoragePointer<u128>,
}

pub impl u256SubPointersImpl of starknet::storage::SubPointers<u256> {
    type SubPointersType = u256SubPointers;
    fn sub_pointers(self: starknet::storage::StoragePointer<u256>) -> u256SubPointers {
        let base_address = self.__storage_pointer_address__;
        let mut current_offset = self.__storage_pointer_offset__;
        let low_value = starknet::storage::StoragePointer::<
            u128,
        > { __storage_pointer_address__: base_address, __storage_pointer_offset__: current_offset };
        current_offset = current_offset + starknet::Store::<u128>::size();
        let high_value = starknet::storage::StoragePointer::<
            u128,
        > { __storage_pointer_address__: base_address, __storage_pointer_offset__: current_offset };

        u256SubPointers { low: low_value, high: high_value }
    }
}

#[derive(Drop, Copy)]
struct U256SubPointersMut {
    pub low: starknet::storage::StoragePointer<Mutable<u128>>,
    pub high: starknet::storage::StoragePointer<Mutable<u128>>,
}

pub impl U256SubPointersImplMut of starknet::storage::SubPointersMut<u256> {
    type SubPointersType = U256SubPointersMut;
    fn sub_pointers_mut(
        self: starknet::storage::StoragePointer<Mutable<u256>>,
    ) -> U256SubPointersMut {
        let base_address = self.__storage_pointer_address__;
        let mut current_offset = self.__storage_pointer_offset__;
        let low_value = starknet::storage::StoragePointer::<
            Mutable<u128>,
        > { __storage_pointer_address__: base_address, __storage_pointer_offset__: current_offset };
        current_offset = current_offset + starknet::Store::<u128>::size();
        let high_value = starknet::storage::StoragePointer::<
            Mutable<u128>,
        > { __storage_pointer_address__: base_address, __storage_pointer_offset__: current_offset };

        U256SubPointersMut { low: low_value, high: high_value }
    }
}
