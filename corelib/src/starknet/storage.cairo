use core::traits::Into;
use core::pedersen::HashState;
use core::hash::HashStateTrait;
use starknet::storage_access::StorageBaseAddress;
use starknet::SyscallResult;
use starknet::storage_access::storage_base_address_from_felt252;


/// A pointer to an address in storage, can be used to read and write values, if the generic type
/// supports it (e.g. basic types like `felt252`).
pub struct StoragePointer<T> {
    pub address: StorageBaseAddress,
    pub offset: u8,
}

impl StoragePointerCopy<T> of Copy<StoragePointer<T>> {}
impl StoragePointerDrop<T> of Drop<StoragePointer<T>> {}

/// Same as `StoragePointer`, but with `offset` 0, which allows for some optimizations.
pub struct StoragePointer0Offset<T> {
    pub address: StorageBaseAddress,
}

impl StoragePointer0OffsetCopy<T> of Copy<StoragePointer0Offset<T>> {}
impl StoragePointer0OffsetDrop<T> of Drop<StoragePointer0Offset<T>> {}

/// Trait for converting a storage member to a `StoragePointer0Offset`.
// type instead of `T`.
pub trait StorageAsPointer<TMemberState> {
    type Value;
    fn as_ptr(self: @TMemberState) -> StoragePointer0Offset<Self::Value>;
}

/// Trait for accessing the values in storage using a `StoragePointer`.
pub trait StoragePointerReadAccess<T> {
    type Value;
    fn read(self: @T) -> Self::Value;
}

/// Trait for writing values to storage using a `StoragePointer`.
pub trait StoragePointerWriteAccess<T> {
    type Value;
    fn write(self: T, value: Self::Value);
}

/// Trait for reading a contract/component storage member in a specific key place.
pub trait StorageMapReadAccessTrait<TMemberState> {
    type Key;
    type Value;
    fn read(self: TMemberState, key: Self::Key) -> Self::Value;
}

/// Trait for writing contract/component storage member in a specific key place.
pub trait StorageMapWriteAccessTrait<TMemberState> {
    type Key;
    type Value;
    fn write(self: TMemberState, key: Self::Key, value: Self::Value);
}


/// Simple implementation of `StoragePointerReadAccess` for any type that implements `Store` for 0
/// offset.
impl StorableStoragePointer0OffsetReadAccess<
    T, +starknet::Store<T>
> of StoragePointerReadAccess<StoragePointer0Offset<T>> {
    type Value = T;
    fn read(self: @StoragePointer0Offset<T>) -> T {
        starknet::SyscallResultTrait::unwrap_syscall(starknet::Store::<T>::read(0, *self.address))
    }
}

/// Simple implementation of `StoragePointerReadAccess` for any type that implements `Store` for 0
/// offset.
impl MutableStorableStoragePointer0OffsetReadAccess<
    T, +MutableTrait<T>, +starknet::Store<MutableTrait::<T>::InnerType>
> of StoragePointerReadAccess<StoragePointer0Offset<T>> {
    type Value = MutableTrait::<T>::InnerType;
    fn read(self: @StoragePointer0Offset<T>) -> MutableTrait::<T>::InnerType {
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<MutableTrait::<T>::InnerType>::read(0, *self.address)
        )
    }
}

/// Simple implementation of `StoragePointerWriteAccess` for any type that implements `Store` for 0
/// offset.
impl StorableStoragePointer0OffsetWriteAccess<
    T, +MutableTrait<T>, +starknet::Store<MutableTrait::<T>::InnerType>
> of StoragePointerWriteAccess<StoragePointer0Offset<T>> {
    type Value = MutableTrait::<T>::InnerType;
    fn write(self: StoragePointer0Offset<T>, value: MutableTrait::<T>::InnerType) {
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<MutableTrait::<T>::InnerType>::write(0, self.address, value)
        )
    }
}

/// Simple implementation of `StoragePointerReadAccess` for any type that implements `Store` for any
/// offset.
pub impl StorableStoragePointerReadAccess<
    T, +starknet::Store<T>
> of StoragePointerReadAccess<StoragePointer<T>> {
    type Value = T;
    fn read(self: @StoragePointer<T>) -> T {
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<T>::read_at_offset(0, *self.address, *self.offset)
        )
    }
}

/// Simple implementation of `StoragePointerReadAccess` for any mutable type that implements `Store`
impl MutableStorableStoragePointerReadAccess<
    T, +MutableTrait<T>, +starknet::Store<MutableTrait::<T>::InnerType>
> of StoragePointerReadAccess<StoragePointer<T>> {
    type Value = MutableTrait::<T>::InnerType;
    fn read(self: @StoragePointer<T>) -> MutableTrait::<T>::InnerType {
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<
                MutableTrait::<T>::InnerType
            >::read_at_offset(0, *self.address, *self.offset)
        )
    }
}

/// Simple implementation of `StoragePointerWriteAccess` for any mutable type that implements
/// `Store`.
impl MutableStorableStoragePointerWriteAccess<
    T, +MutableTrait<T>, +starknet::Store<MutableTrait::<T>::InnerType>
> of StoragePointerWriteAccess<StoragePointer<T>> {
    type Value = MutableTrait::<T>::InnerType;
    fn write(self: StoragePointer<T>, value: MutableTrait::<T>::InnerType) {
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<
                MutableTrait::<T>::InnerType
            >::write_at_offset(0, self.address, self.offset, value)
        )
    }
}

/// An intermediate struct to store a hash state, in order to be able to hash multiple values and
/// get the final address.
/// Storage path should have two interfaces, if T is storable then it should implement
/// `StorageAsPointer` in order to be able to get the address of the storage path. Otherwise, if
/// T is not storable then it should implement some kind of updating trait, e.g. `StoragePathEntry`.
pub struct StoragePath<T> {
    hash_state: StoragePathHashState,
}

/// The hash state of a storage path.
type StoragePathHashState = core::pedersen::HashState;

impl StoragePathCopy<T> of core::traits::Copy<StoragePath<T>> {}
impl StoragePathDrop<T> of core::traits::Drop<StoragePath<T>> {}

/// Trait for StoragePath operations.
trait StoragePathTrait<T> {
    fn new(init_value: felt252) -> StoragePath<T>;
    fn finalize(self: StoragePath<T>) -> StorageBaseAddress;
}


impl StoragePathImpl<T> of StoragePathTrait<T> {
    fn new(init_value: felt252) -> StoragePath<T> {
        StoragePath { hash_state: core::pedersen::PedersenTrait::new(init_value) }
    }
    fn finalize(self: StoragePath<T>) -> StorageBaseAddress {
        storage_base_address_from_felt252(self.hash_state.finalize())
    }
}


/// Trait for creating a new `StoragePath` from a storage member.
pub trait StorageAsPath<TMemberState> {
    type Value;
    fn as_path(self: @TMemberState) -> StoragePath<Self::Value>;
}

/// An implementation of `StorageAsPointer` for any `StoragePath` with inner type that implements
/// `Store`.
impl StorableStoragePathAsPointer<T, +starknet::Store<T>> of StorageAsPointer<StoragePath<T>> {
    type Value = T;
    fn as_ptr(self: @StoragePath<T>) -> StoragePointer0Offset<T> {
        StoragePointer0Offset { address: (*self).finalize() }
    }
}

/// An implementation of `StorageAsPointer` for any `StoragePath` with inner type that implements
/// `Store`.
impl MutableStorableStoragePathAsPointer<
    T, +MutableTrait<T>, +starknet::Store<MutableTrait::<T>::InnerType>
> of StorageAsPointer<StoragePath<T>> {
    type Value = T;
    fn as_ptr(self: @StoragePath<T>) -> StoragePointer0Offset<T> {
        StoragePointer0Offset { address: (*self).finalize() }
    }
}


/// Trait for updating the hash state with a value, using an `entry` method.
pub trait StoragePathEntry<C> {
    type Key;
    type Value;
    fn entry(self: C, key: Self::Key) -> StoragePath<Self::Value>;
}

/// A struct that represents a map in a contract storage.
// TODO(Gil): Make it a phantom type once we can annotate a type as phantom from within another
// attribute, specifically from #[storage].
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

/// Implement StoragePathEntry for any `EntryInfo` type if their key implements `Hash`.
impl EntryInfoStoragePathEntry<
    T, +EntryInfo<T>, +core::hash::Hash<EntryInfo::<T>::Key, StoragePathHashState>
> of StoragePathEntry<StoragePath<T>> {
    type Key = EntryInfo::<T>::Key;
    type Value = EntryInfo::<T>::Value;
    fn entry(self: StoragePath<T>, key: EntryInfo::<T>::Key) -> StoragePath<EntryInfo::<T>::Value> {
        StoragePath::<
            EntryInfo::<T>::Value
        > {
            hash_state: core::hash::Hash::<
                EntryInfo::<T>::Key, StoragePathHashState
            >::update_state(self.hash_state, key)
        }
    }
}

/// Same as `StoragePathEntryMap`, but for Mutable<T>, forwards the Mutable wrapper onto the value
/// type.
impl MutableEntryStoragePathEntry<
    T,
    +MutableTrait<T>,
    impl EntryImpl: EntryInfo<MutableTrait::<T>::InnerType>,
    +core::hash::Hash<EntryImpl::Key, StoragePathHashState>
> of StoragePathEntry<StoragePath<T>> {
    type Key = EntryImpl::Key;
    type Value = Mutable<EntryImpl::Value>;
    fn entry(self: StoragePath<T>, key: EntryImpl::Key) -> StoragePath<Mutable<EntryImpl::Value>> {
        StoragePath::<
            Mutable<EntryImpl::Value>
        > {
            hash_state: core::hash::Hash::<
                EntryImpl::Key, StoragePathHashState
            >::update_state(self.hash_state, key)
        }
    }
}

/// Implement StorageMapAccessTrait for any type that implements StoragePathEntry and Store.
impl StorableEntryReadAccess<
    T,
    +EntryInfo<T>,
    +core::hash::Hash<EntryInfo::<T>::Key, StoragePathHashState>,
    +starknet::Store<EntryInfo::<T>::Value>,
> of StorageMapReadAccessTrait<StoragePath<T>> {
    type Key = EntryInfo::<T>::Key;
    type Value = EntryInfo::<T>::Value;
    fn read(self: StoragePath<T>, key: EntryInfo::<T>::Key) -> EntryInfo::<T>::Value {
        self.entry(key).as_ptr().read()
    }
}

impl StorageAsPathReadForward<
    T,
    impl PathImpl: StorageAsPath<T>,
    impl AccessImpl: StorageMapReadAccessTrait<StoragePath<PathImpl::Value>>,
    +Drop<T>,
    +Drop<AccessImpl::Key>,
> of StorageMapReadAccessTrait<T> {
    type Key = AccessImpl::Key;
    type Value = AccessImpl::Value;
    #[inline(always)]
    fn read(self: T, key: AccessImpl::Key) -> AccessImpl::Value {
        self.as_path().read(key)
    }
}

/// Implement StorageMapAccessTrait for any Mutable type that implements StoragePathEntry and
/// Store.
impl MutableStorableEntryReadAccess<
    T,
    +MutableTrait<T>,
    +EntryInfo<MutableTrait::<T>::InnerType>,
    +core::hash::Hash<EntryInfo::<MutableTrait::<T>::InnerType>::Key, StoragePathHashState>,
    +starknet::Store<EntryInfo::<MutableTrait::<T>::InnerType>::Value>,
> of StorageMapReadAccessTrait<StoragePath<T>> {
    type Key = EntryInfo::<MutableTrait::<T>::InnerType>::Key;
    type Value = EntryInfo::<MutableTrait::<T>::InnerType>::Value;
    #[inline(always)]
    fn read(
        self: StoragePath<T>, key: EntryInfo::<MutableTrait::<T>::InnerType>::Key
    ) -> EntryInfo::<MutableTrait::<T>::InnerType>::Value {
        self.entry(key).as_ptr().read()
    }
}


/// Implement StorageMapAccessTrait for any Mutable type that implements StoragePathEntry and
/// Store.
impl MutableStorableEntryWriteAccess<
    T,
    +MutableTrait<T>,
    +EntryInfo<MutableTrait::<T>::InnerType>,
    +core::hash::Hash<EntryInfo::<MutableTrait::<T>::InnerType>::Key, StoragePathHashState>,
    +starknet::Store<EntryInfo::<MutableTrait::<T>::InnerType>::Value>,
    +Drop<EntryInfo::<MutableTrait::<T>::InnerType>::Value>
> of StorageMapWriteAccessTrait<StoragePath<T>> {
    type Key = EntryInfo::<MutableTrait::<T>::InnerType>::Key;
    type Value = EntryInfo::<MutableTrait::<T>::InnerType>::Value;
    fn write(
        self: StoragePath<T>,
        key: EntryInfo::<MutableTrait::<T>::InnerType>::Key,
        value: EntryInfo::<MutableTrait::<T>::InnerType>::Value
    ) {
        self.entry(key).as_ptr().write(value)
    }
}


impl StorageAsPathWriteForward<
    T,
    impl PathImpl: StorageAsPath<T>,
    impl AccessImpl: StorageMapWriteAccessTrait<StoragePath<PathImpl::Value>>,
    +Drop<T>,
    +Drop<AccessImpl::Key>,
    +Drop<AccessImpl::Value>,
> of StorageMapWriteAccessTrait<T> {
    type Key = AccessImpl::Key;
    type Value = AccessImpl::Value;
    fn write(self: T, key: AccessImpl::Key, value: AccessImpl::Value) {
        self.as_path().write(key, value)
    }
}

/// A trait that binds a storage path of a struct, and the struct storage node (a storage node is a
/// struct that all its fields are storage paths, one for each member of the original struct).
pub trait StorageNode<T> {
    type NodeType;
    fn storage_node(self: StoragePath<T>) -> Self::NodeType;
}

/// This makes the storage node members directly accessible from a path to the parent struct.
impl StorageNodeDeref<T, +StorageNode<T>> of core::ops::Deref<StoragePath<T>> {
    type Target = StorageNode::<T>::NodeType;
    fn deref(self: StoragePath<T>) -> Self::Target {
        self.storage_node()
    }
}

/// A mutable version of `StorageNode`, works the same way, but on `Mutable<T>`.
pub trait MutableStorageNode<T> {
    type NodeType;
    fn mutable_storage_node(self: StoragePath<Mutable<T>>) -> Self::NodeType;
}

/// This makes the storage node members directly accessible from a path to the parent struct.
impl MutableStorageNodeDeref<
    T, +MutableStorageNode<T>
> of core::ops::Deref<StoragePath<Mutable<T>>> {
    type Target = MutableStorageNode::<T>::NodeType;
    fn deref(self: StoragePath<Mutable<T>>) -> Self::Target {
        self.mutable_storage_node()
    }
}

/// Similar to storage node, but for structs which are stored sequentially in the storage. In
/// contrast to storage node, the fields of the struct are just offsetted from the base address of
/// the struct.
pub trait SubPointers<T> {
    /// The type of the storage pointers, generated for the struct T.
    type SubPointersType;
    /// Creates a sub pointers struct for the given storage pointer to a struct T.
    fn sub_pointers(self: StoragePointer<T>) -> Self::SubPointersType;
}

/// This makes the sub-pointers members directly accessible from a pointer to the parent struct.
impl SubPointersDeref<T, +SubPointers<T>> of core::ops::Deref<StoragePointer<T>> {
    type Target = SubPointers::<T>::SubPointersType;
    fn deref(self: StoragePointer<T>) -> Self::Target {
        self.sub_pointers()
    }
}

/// A mutable version of `SubPointers`, works the same way, but on `Mutable<T>`.
pub trait MutableSubPointers<T> {
    /// The type of the storage pointers, generated for the struct T.
    type SubPointersType;
    /// Creates a sub pointers struct for the given storage pointer to a struct T.
    fn mutable_sub_pointers(self: StoragePointer<Mutable<T>>) -> Self::SubPointersType;
}

/// This makes the sub-pointers members directly accessible from a pointer to the parent struct.
impl MutableSubPointersDeref<
    T, +MutableSubPointers<T>
> of core::ops::Deref<StoragePointer<Mutable<T>>> {
    type Target = MutableSubPointers::<T>::SubPointersType;
    fn deref(self: StoragePointer<Mutable<T>>) -> Self::Target {
        self.mutable_sub_pointers()
    }
}


/// Implement deref for storage paths that implements StorageAsPointer.
impl StoragePathDeref<
    T, impl PointerImpl: StorageAsPointer<StoragePath<T>>
> of core::ops::Deref<StoragePath<T>> {
    type Target = StoragePointer0Offset<PointerImpl::Value>;
    fn deref(self: StoragePath<T>) -> StoragePointer0Offset<PointerImpl::Value> {
        self.as_ptr()
    }
}

/// Implement deref for StoragePointer0Offset into a StoragePointer.
impl StoragePointer0OffsetDeref<T> of core::ops::Deref<StoragePointer0Offset<T>> {
    type Target = StoragePointer<T>;
    fn deref(self: StoragePointer0Offset<T>) -> StoragePointer<T> {
        StoragePointer::<T> { address: self.address, offset: 0 }
    }
}


/// A struct for delaying the creation of a storage path, used for lazy evaluation in storage nodes.
pub struct PendingStoragePath<T> {
    hash_state: StoragePathHashState,
    pending_key: felt252,
}

/// A trait for creating a `PendingStoragePath` from a hash state and a key.
pub trait PendingStoragePathTrait<T, S> {
    fn new(storage_path: @StoragePath<S>, pending_key: felt252) -> PendingStoragePath<T>;
}

/// An implementation of `StoragePathEntry` for `PendingStoragePath`.
impl PendingStoragePathImpl<T, S> of PendingStoragePathTrait<T, S> {
    fn new(storage_path: @StoragePath<S>, pending_key: felt252) -> PendingStoragePath<T> {
        PendingStoragePath { hash_state: *storage_path.hash_state, pending_key }
    }
}

impl PendingStoragePathDrop<T> of Drop<PendingStoragePath<T>> {}
impl PendingStoragePathCopy<T> of Copy<PendingStoragePath<T>> {}

/// An implementation of 'StorageAsPath' for `PendingStoragePath`.
impl PendingStoragePathAsPath<T> of StorageAsPath<PendingStoragePath<T>> {
    type Value = T;
    fn as_path(self: @PendingStoragePath<T>) -> StoragePath<T> {
        StoragePath::<
            T
        > { hash_state: core::hash::HashStateTrait::update(*self.hash_state, *self.pending_key) }
    }
}

/// Deref pending storage path into a storage path.
impl PendingStoragePathDeref<T> of core::ops::Deref<PendingStoragePath<T>> {
    type Target = StoragePath<T>;
    fn deref(self: PendingStoragePath<T>) -> Self::Target {
        self.as_path()
    }
}

/// A struct for holding an address to initialize a storage path with. The members (not direct
/// members, but accessible using deref) of the contract state are `StorageBase` instances, with the
/// generic type representing the type of the stored member.
pub struct StorageBase<T> {
    pub address: felt252,
}

/// A trait for creating the struct containing the storage base of all the members of a contract
/// state.
pub trait StorageBaseTrait<T> {
    type BaseType;
    type BaseMutType;
    /// Creates a struct containing the storage base of all the members of a contract state. Should
    /// be called from the `deref` method of the contract state.
    fn storage_base(self: @T) -> Self::BaseType;
    /// Creates a struct containing the storage base, with the generic type wrapped in a `Mutable`
    /// type. Should be called from the `deref_mut` method of the contract state.
    fn storage_base_mut(ref self: T) -> Self::BaseMutType;
}

impl StorageBaseDrop<T> of Drop<StorageBase<T>> {}
impl StorageBaseCopy<T> of Copy<StorageBase<T>> {}

impl StorageBaseAsPath<T> of StorageAsPath<StorageBase<T>> {
    type Value = T;
    fn as_path(self: @StorageBase<T>) -> StoragePath<T> {
        StoragePathTrait::new(*self.address)
    }
}

impl StorageBaseDeref<T> of core::ops::Deref<StorageBase<T>> {
    type Target = StoragePath<T>;
    fn deref(self: StorageBase<T>) -> Self::Target {
        self.as_path()
    }
}


/// Implement as_ptr for any type that implements StorageAsPath and Store.
impl StorablePathableStorageAsPointer<
    T,
    impl PathImpl: StorageAsPath<T>,
    impl PtrImpl: StorageAsPointer<StoragePath<PathImpl::Value>>,
> of StorageAsPointer<T> {
    type Value = PtrImpl::Value;
    fn as_ptr(self: @T) -> StoragePointer0Offset<PtrImpl::Value> {
        let path = self.as_path();
        path.as_ptr()
    }
}

/// Implement StoragePointerReadAccess for any type that implements StorageAsPointer and
/// StoragePointerReadAccess.
impl StorablePointerReadAccessImpl<
    T,
    impl PointerImpl: StorageAsPointer<T>,
    impl AccessImpl: StoragePointerReadAccess<StoragePointer0Offset<PointerImpl::Value>>,
> of StoragePointerReadAccess<T> {
    type Value = AccessImpl::Value;
    fn read(self: @T) -> Self::Value {
        self.as_ptr().read()
    }
}

/// Implement StoragePointerWriteAccess for any type that implements StorageAsPointer.
impl StorablePointerWriteAccessImpl<
    T,
    impl PointerImpl: StorageAsPointer<T>,
    impl AccessImpl: StoragePointerWriteAccess<StoragePointer0Offset<PointerImpl::Value>>,
    +Drop<T>,
    +Drop<AccessImpl::Value>,
> of StoragePointerWriteAccess<T> {
    type Value = AccessImpl::Value;
    fn write(self: T, value: Self::Value) {
        let ptr: StoragePointer0Offset<PointerImpl::Value> = self.as_ptr();
        ptr.write(value)
    }
}


/// Implement StoragePathEntry for any type that implements StoragePath and StoragePathEntry.
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

/// A wrapper around different storage related types, indicating that the instance is mutable,
/// i.e. originally created from a `ref` contract state.
#[phantom]
pub struct Mutable<T> {}

impl MutableDrop<T> of Drop<Mutable<T>> {}
impl MutableCopy<T> of Copy<Mutable<T>> {}


/// A trait for exposing the inner type of a `Mutable` type.
trait MutableTrait<T> {
    type InnerType;
}

impl MutableImpl<T> of MutableTrait<Mutable<T>> {
    type InnerType = T;
}

/// Implementation of SubPointers for core types.
#[derive(Drop, Copy)]
struct u256SubPointers {
    pub low: starknet::storage::StoragePointer<u128>,
    pub high: starknet::storage::StoragePointer<u128>,
}

impl u256SubPointersImpl of starknet::storage::SubPointers<u256> {
    type SubPointersType = u256SubPointers;
    fn sub_pointers(self: starknet::storage::StoragePointer<u256>) -> u256SubPointers {
        let base_address = self.address;
        let mut current_offset = self.offset;
        let low_value = starknet::storage::StoragePointer::<
            u128
        > { address: base_address, offset: current_offset, };
        current_offset = current_offset + starknet::Store::<u128>::size();
        let high_value = starknet::storage::StoragePointer::<
            u128
        > { address: base_address, offset: current_offset, };

        u256SubPointers { low: low_value, high: high_value, }
    }
}

#[derive(Drop, Copy)]
struct MutableU256SubPointers {
    pub low: starknet::storage::StoragePointer<Mutable<u128>>,
    pub high: starknet::storage::StoragePointer<Mutable<u128>>,
}

impl MutableU256SubPointersImpl of starknet::storage::MutableSubPointers<u256> {
    type SubPointersType = MutableU256SubPointers;
    fn mutable_sub_pointers(
        self: starknet::storage::StoragePointer<Mutable<u256>>
    ) -> MutableU256SubPointers {
        let base_address = self.address;
        let mut current_offset = self.offset;
        let low_value = starknet::storage::StoragePointer::<
            Mutable<u128>
        > { address: base_address, offset: current_offset, };
        current_offset = current_offset + starknet::Store::<u128>::size();
        let high_value = starknet::storage::StoragePointer::<
            Mutable<u128>
        > { address: base_address, offset: current_offset, };

        MutableU256SubPointers { low: low_value, high: high_value, }
    }
}
