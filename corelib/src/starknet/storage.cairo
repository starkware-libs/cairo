use core::traits::Into;
use core::poseidon::HashState;
use core::hash::HashStateTrait;
use starknet::storage_access::StorageBaseAddress;
use starknet::SyscallResult;
use starknet::storage_access::storage_base_address_from_felt252;

/// Trait for getting the address of any contract/component storage member.
pub trait StorageMemberAddressTrait<TMemberState> {
    /// the type of the underlying storage member.
    type Value;
    fn address(self: @TMemberState) -> starknet::StorageBaseAddress nopanic;
}

/// Trait for accessing any contract/component storage member.
pub trait StorageMemberAccessTrait<TMemberState> {
    type Value;
    fn read(self: @TMemberState) -> Self::Value;
    fn write(ref self: TMemberState, value: Self::Value);
}

/// Implementation of StorageMemberAccessTrait for types that implement StorageMemberAddressTrait.
pub impl StorageMemberAccessImpl<
    TMemberState,
    +StorageMemberAddressTrait<TMemberState>,
    +starknet::Store<StorageMemberAddressTrait::<TMemberState>::Value>,
    +Drop<TMemberState>,
> of StorageMemberAccessTrait<TMemberState> {
    type Value = StorageMemberAddressTrait::<TMemberState>::Value;
    fn read(self: @TMemberState) -> Self::Value {
        // Only address_domain 0 is currently supported.
        let address_domain = 0_u32;
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<Self::Value>::read(address_domain, self.address())
        )
    }
    fn write(ref self: TMemberState, value: Self::Value) {
        // Only address_domain 0 is currently supported.
        let address_domain = 0_u32;
        let write_result = starknet::Store::<
            Self::Value
        >::write(address_domain, self.address(), value);
        starknet::SyscallResultTrait::unwrap_syscall(write_result)
    }
}

/// Trait for getting the address of a contract/component legacy map storage member.
pub trait StorageLegacyMapMemberAddressTrait<TMemberState> {
    type Key;
    type Value;
    fn address(self: @TMemberState, key: Self::Key) -> starknet::StorageBaseAddress;
}

/// Trait for accessing any contract/component storage member.
pub trait StorageLegacyMapMemberAccessTrait<TMemberState> {
    type Key;
    type Value;
    fn read(self: @TMemberState, key: Self::Key) -> Self::Value;
    fn write(ref self: TMemberState, key: Self::Key, value: Self::Value);
}

/// Trait for getting the address of a contract/component map storage member.
pub trait StorageMapMemberAddressTrait<TMemberState> {
    type Key;
    type Value;
    fn address(self: @TMemberState) -> starknet::StorageBaseAddress;
}

/// Implementation of StorageLegacyMapMemberAccessTrait for types that implement
/// StorageLegacyMapMemberAddressTrait.
pub impl StorageLegacyMapMemberAccessImpl<
    TMemberState,
    +StorageLegacyMapMemberAddressTrait<TMemberState>,
    +starknet::Store<StorageLegacyMapMemberAddressTrait::<TMemberState>::Value>,
    +Drop<TMemberState>,
    +PanicDestruct<StorageLegacyMapMemberAddressTrait::<TMemberState>::Value>,
> of StorageLegacyMapMemberAccessTrait<TMemberState> {
    type Key = StorageLegacyMapMemberAddressTrait::<TMemberState>::Key;
    type Value = StorageLegacyMapMemberAddressTrait::<TMemberState>::Value;
    fn read(self: @TMemberState, key: Self::Key) -> Self::Value {
        // Only address_domain 0 is currently supported.
        let address_domain = 0_u32;
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<Self::Value>::read(address_domain, self.address(key))
        )
    }
    fn write(ref self: TMemberState, key: Self::Key, value: Self::Value) {
        // Only address_domain 0 is currently supported.
        let address_domain = 0_u32;
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<Self::Value>::write(address_domain, self.address(key), value)
        )
    }
}


/// A pointer to an address in storage, can be used to read and write values, if the generic type
/// supports it (e.g. basic types like `felt252`).
#[derive(Copy, Drop)]
pub struct StoragePointer<T> {
    pub address: StorageBaseAddress,
    pub offset: u8,
}

/// Same as `StoragePointer`, but with `offset` 0, which allows for some optimizations.
#[derive(Copy, Drop)]
pub struct StoragePointer0Offset<T> {
    pub address: StorageBaseAddress,
}

/// Trait for converting a storage member to a `StoragePointer0Offset`.
// type instead of `T`.
pub trait StorageAsPointer<TMemberState> {
    type Value;
    fn as_ptr(self: @TMemberState) -> StoragePointer0Offset<Self::Value>;
}

/// Trait for accessing the values in storage using a `StoragePointer`.
pub trait StoragePointerAccess<T> {
    type Value;
    fn read(self: T) -> Self::Value;
    fn write(self: T, value: Self::Value);
}

/// Simple implementation of `StoragePointerAccess` for any type that implements `Store` for 0
/// offset.
impl StorableStoragePointer0OffsetAccess<
    T, +starknet::Store<T>
> of StoragePointerAccess<StoragePointer0Offset<T>> {
    type Value = T;
    #[inline(always)]
    fn read(self: StoragePointer0Offset<T>) -> T {
        starknet::SyscallResultTrait::unwrap_syscall(starknet::Store::<T>::read(0, self.address))
    }
    #[inline(always)]
    fn write(self: StoragePointer0Offset<T>, value: T) {
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<T>::write(0, self.address, value)
        )
    }
}

/// Simple implementation of `StoragePointerAccess` for any type that implements `Store` for any
/// offset.
impl StorableStoragePointerAccess<
    T, +starknet::Store<T>
> of StoragePointerAccess<StoragePointer<T>> {
    type Value = T;
    #[inline(always)]
    fn read(self: StoragePointer<T>) -> T {
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<T>::read_at_offset(0, self.address, self.offset)
        )
    }
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
    pub hash_state: StoragePathHashState,
}

/// The hash state of a storage path.
type StoragePathHashState = core::poseidon::HashState;

impl StoragePathCopy<T> of core::traits::Copy<StoragePath<T>> {}
impl StoragePathDrop<T> of core::traits::Drop<StoragePath<T>> {}

/// Trait for StoragePath operations.
trait StoragePathTrait<T> {
    fn new(init_value: felt252) -> StoragePath<T>;
    fn finalize(self: StoragePath<T>) -> StorageBaseAddress;
}


impl StoragePathImpl<T> of StoragePathTrait<T> {
    fn new(init_value: felt252) -> StoragePath<T> {
        StoragePath {
            hash_state: core::hash::HashStateTrait::update(
                core::poseidon::PoseidonTrait::new(), init_value
            )
        }
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

/// An implementation of `StorageAsPointer` for any type that implements `StorageMemberAccessTrait`
/// and `Store`.
impl StorageMemberStateAsPointer<
    TMemberState,
    +StorageMemberAddressTrait<TMemberState>,
    +starknet::Store<StorageMemberAddressTrait::<TMemberState>::Value>,
> of StorageAsPointer<TMemberState> {
    type Value = StorageMemberAddressTrait::<TMemberState>::Value;
    fn as_ptr(self: @TMemberState) -> StoragePointer0Offset<Self::Value> {
        StoragePointer0Offset { address: self.address() }
    }
}

/// Trait for updating the hash state with a value, using an `entry` method.
pub trait StoragePathEntry<C> {
    type Key;
    type Value;
    fn entry(self: StoragePath<C>, key: Self::Key) -> StoragePath<Self::Value>;
}

/// A struct that represents a map in a contract storage.
#[phantom]
pub struct Map<K, V> {}

impl StoragePathEntryMap<
    K, V, +core::hash::Hash<K, StoragePathHashState>
> of StoragePathEntry<Map<K, V>> {
    type Key = K;
    type Value = V;
    #[inline(always)]
    fn entry(self: StoragePath<Map<K, V>>, key: K) -> StoragePath<V> {
        StoragePath::<
            V
        > {
            hash_state: core::hash::Hash::<
                K, StoragePathHashState
            >::update_state(self.hash_state, key)
        }
    }
}

/// A trait that binds a storage path to a struct, and the struct storage node (a storage node is a
/// struct that all its fields are storage paths, one for each member of the original struct).
trait StorageNodeTrait<T> {
    type NodeType;
    fn storage_node(self: StoragePath<T>) -> Self::NodeType;
}

impl StorageNodeDeref<T, +StorageNodeTrait<T>> of core::ops::Deref<StoragePath<T>> {
    type Target = StorageNodeTrait::<T>::NodeType;
    fn deref(self: StoragePath<T>) -> Self::Target {
        self.storage_node()
    }
}


/// A struct for delaying the creation of a storage path, used for lazy evaluation in storage nodes.
#[derive(Copy, Drop)]
struct PendingStoragePath<T> {
    hash_state: StoragePathHashState,
    pending_key: felt252
}

/// An implementation of 'StorageAsPath' for `PendingStoragePath`.
impl PendingStoragePathAsPath<T> of StorageAsPath<PendingStoragePath<T>> {
    type Value = T;
    fn as_path(self: @PendingStoragePath<T>) -> StoragePath<T> {
        StoragePath::<
            T
        > { hash_state: core::hash::HashStateTrait::update(*self.hash_state, *self.pending_key) }
    }
}


/// An implementation of `StorageAsPath` for any type that implements StorageNodeTrait.
impl StorageNodeAsPath<
    TMemberState,
    +StorageMemberAddressTrait<TMemberState>,
    +StorageNodeTrait<StorageMemberAddressTrait::<TMemberState>::Value>,
> of StorageAsPath<TMemberState> {
    type Value = StorageMemberAddressTrait::<TMemberState>::Value;
    fn as_path(self: @TMemberState) -> StoragePath<Self::Value> {
        StoragePathTrait::new(self.address().into())
    }
}

impl StorageMemberDeref<
    TMemberState,
    +StorageMemberAddressTrait<TMemberState>,
    +StorageNodeTrait<StorageMemberAddressTrait::<TMemberState>::Value>,
    +Drop<TMemberState>,
> of core::ops::Deref<TMemberState> {
    type Target = StoragePath<StorageMemberAddressTrait::<TMemberState>::Value>;
    fn deref(self: TMemberState) -> Self::Target {
        self.as_path()
    }
}

/// An implementation of `StorageAsPath` for `Map<K, V>`.
impl MapAsPath<
    TMemberState, +StorageMapMemberAddressTrait<TMemberState>
> of StorageAsPath<TMemberState> {
    type Value =
        Map<
            StorageMapMemberAddressTrait::<TMemberState>::Key,
            StorageMapMemberAddressTrait::<TMemberState>::Value
        >;
    fn as_path(self: @TMemberState) -> StoragePath<Self::Value> {
        StoragePathTrait::new(self.address().into())
    }
}

