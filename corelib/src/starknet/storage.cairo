use core::traits::Into;
use core::pedersen::HashState;
use core::hash::HashStateTrait;
use starknet::storage_access::StorageBaseAddress;
use starknet::SyscallResult;
use starknet::storage_access::storage_base_address_from_felt252;


/// A pointer to an address in storage, can be used to read and write values, if the generic type
/// supports it (e.g. basic types like `felt252`).
pub struct StoragePointer<T> {
    address: StorageBaseAddress,
    offset: u8,
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
    #[inline(always)]
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
    #[inline(always)]
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
    #[inline(always)]
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
    #[inline(always)]
    fn read(self: @StoragePointer<T>) -> T {
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<T>::read_at_offset(0, *self.address, *self.offset)
        )
    }
}

/// Simple implementation of `StoragePointerWriteAccess` for any type that implements `Store` for
/// any offset.
pub impl StorableStoragePointerWriteAccess<
    T, +starknet::Store<T>
> of StoragePointerWriteAccess<StoragePointer<T>> {
    type Value = T;
    #[inline(always)]
    fn write(self: StoragePointer<T>, value: T) {
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<T>::write_at_offset(0, self.address, self.offset, value)
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
pub struct Map<K, V> {}

/// A trait for making a map like type support implement the `StoragePathEntry` trait.
trait Entryable<T> {
    type Key;
    type Value;
}

impl EntryableImpl<K, V> of Entryable<Map<K, V>> {
    type Key = K;
    type Value = V;
}

/// Implement StoragePathEntry for any `Entryable` type with a key that implements `Hash`.
impl EntryableStoragePathEntry<
    T, +Entryable<T>, +core::hash::Hash<Entryable::<T>::Key, StoragePathHashState>
> of StoragePathEntry<StoragePath<T>> {
    type Key = Entryable::<T>::Key;
    type Value = Entryable::<T>::Value;
    #[inline(always)]
    fn entry(self: StoragePath<T>, key: Entryable::<T>::Key) -> StoragePath<Entryable::<T>::Value> {
        StoragePath::<
            Entryable::<T>::Value
        > {
            hash_state: core::hash::Hash::<
                Entryable::<T>::Key, StoragePathHashState
            >::update_state(self.hash_state, key)
        }
    }
}

/// Same as `StoragePathEntryMap`, but for Mutable<T>, forwards the Mutable wrapper onto the value
/// type.
impl MutableEntryStoragePathEntry<
    T,
    +MutableTrait<T>,
    impl EntryImpl: Entryable<MutableTrait::<T>::InnerType>,
    +core::hash::Hash<EntryImpl::Key, StoragePathHashState>
> of StoragePathEntry<StoragePath<T>> {
    type Key = EntryImpl::Key;
    type Value = Mutable<EntryImpl::Value>;
    #[inline(always)]
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
    +Entryable<T>,
    +core::hash::Hash<Entryable::<T>::Key, StoragePathHashState>,
    +starknet::Store<Entryable::<T>::Value>,
    +Drop<Entryable::<T>::Key>,
    +Drop<Entryable::<T>::Value>
> of StorageMapReadAccessTrait<StoragePath<T>> {
    type Key = Entryable::<T>::Key;
    type Value = Entryable::<T>::Value;
    #[inline(always)]
    fn read(self: StoragePath<T>, key: Entryable::<T>::Key) -> Entryable::<T>::Value {
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
    +Entryable<MutableTrait::<T>::InnerType>,
    +core::hash::Hash<Entryable::<MutableTrait::<T>::InnerType>::Key, StoragePathHashState>,
    +starknet::Store<Entryable::<MutableTrait::<T>::InnerType>::Value>,
    +Drop<Entryable::<MutableTrait::<T>::InnerType>::Key>,
    +Drop<Entryable::<MutableTrait::<T>::InnerType>::Value>
> of StorageMapReadAccessTrait<StoragePath<T>> {
    type Key = Entryable::<MutableTrait::<T>::InnerType>::Key;
    type Value = Entryable::<MutableTrait::<T>::InnerType>::Value;
    #[inline(always)]
    fn read(
        self: StoragePath<T>, key: Entryable::<MutableTrait::<T>::InnerType>::Key
    ) -> Entryable::<MutableTrait::<T>::InnerType>::Value {
        self.entry(key).as_ptr().read()
    }
}


/// Implement StorageMapAccessTrait for any Mutable type that implements StoragePathEntry and
/// Store.
impl MutableStorableEntryWriteAccess<
    T,
    +MutableTrait<T>,
    +Entryable<MutableTrait::<T>::InnerType>,
    +core::hash::Hash<Entryable::<MutableTrait::<T>::InnerType>::Key, StoragePathHashState>,
    +starknet::Store<Entryable::<MutableTrait::<T>::InnerType>::Value>,
    +Drop<Entryable::<MutableTrait::<T>::InnerType>::Key>,
    +Drop<Entryable::<MutableTrait::<T>::InnerType>::Value>
> of StorageMapWriteAccessTrait<StoragePath<T>> {
    type Key = Entryable::<MutableTrait::<T>::InnerType>::Key;
    type Value = Entryable::<MutableTrait::<T>::InnerType>::Value;
    #[inline(always)]
    fn write(
        self: StoragePath<T>,
        key: Entryable::<MutableTrait::<T>::InnerType>::Key,
        value: Entryable::<MutableTrait::<T>::InnerType>::Value
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
    #[inline(always)]
    fn write(self: T, key: AccessImpl::Key, value: AccessImpl::Value) {
        self.as_path().write(key, value)
    }
}

/// A trait that binds a storage path to a struct, and the struct storage node (a storage node is a
/// struct that all its fields are storage paths, one for each member of the original struct).
pub trait StorageNode<T> {
    type NodeType;
    fn storage_node(self: StoragePath<T>) -> Self::NodeType;
}

impl StorageNodeDeref<T, +StorageNode<T>> of core::ops::Deref<StoragePath<T>> {
    type Target = StorageNode::<T>::NodeType;
    fn deref(self: StoragePath<T>) -> Self::Target {
        self.storage_node()
    }
}

/// A struct for delaying the creation of a storage path, used for lazy evaluation in storage nodes.
struct PendingStoragePath<T> {
    hash_state: StoragePathHashState,
    pending_key: felt252
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
    +Drop<T>,
    +Drop<AccessImpl::Value>,
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
