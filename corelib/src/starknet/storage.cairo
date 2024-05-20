use core::traits::Into;
use core::poseidon::HashState;
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
}

/// Trait for converting a storage member to a `StoragePointer`.
// TODO(Gil): Once associated types are stabilized, use a trait of `TMemberState` with an associated
// type instead of `T`.
pub trait StorageAsPointer<TMemberState, T> {
    fn as_ptr(self: @TMemberState) -> StoragePointer<T>;
}

/// Trait for accessing the values in storage using a `StoragePointer`.
pub trait StoragePointerAccess<T> {
    fn read(self: StoragePointer<T>) -> T;
    fn write(self: StoragePointer<T>, value: T);
}

/// Simple implementation of `StoragePointerAccess` for any type that implements `Store`.
impl StorableStoragePointerAccess<T, +starknet::Store<T>> of StoragePointerAccess<T> {
    #[inline(always)]
    fn read(self: StoragePointer<T>) -> T {
        starknet::SyscallResultTrait::unwrap_syscall(starknet::Store::<T>::read(0, self.address))
    }
    #[inline(always)]
    fn write(self: StoragePointer<T>, value: T) {
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<T>::write(0, self.address, value)
        )
    }
}

/// An intermediate struct to store a hash state, in order to be able to hash multiple values and
/// get the final address.
/// Storage path should have two interfaces, if T is storable then it should implement
/// `StorageAsPointer` in order to be able to get the address of the storage path. Otherwise, if
/// T is not storable then it should implement some kind of updating trait, e.g. `StoragePathEntry`.
#[derive(Copy, Drop)]
pub struct GenericStoragePath<T, THashState> {
    pub hash_state: THashState,
}

/// A 'GenericStoragePath' that uses the Poseidon hash function. Used as the default storage path.
type StoragePath<T> = GenericStoragePath<T, core::poseidon::HashState>;

/// Trait for creating a new `StoragePath` from a storage member.
pub trait StorageAsPath<TMemberState> {
    type Value;
    fn as_path(self: @TMemberState) -> StoragePath<Self::Value>;
}

/// An implementation of `StorageAsPointer` for any `StoragePath` with inner type that implements
/// `Store`.
impl StorableStoragePathAsPointer<T, +starknet::Store<T>> of StorageAsPointer<StoragePath<T>, T> {
    fn as_ptr(self: @StoragePath<T>) -> StoragePointer<T> {
        StoragePointer::<
            T
        > {
            address: starknet::storage_access::storage_base_address_from_felt252(
                core::hash::HashStateTrait::<core::poseidon::HashState>::finalize(*self.hash_state)
            )
        }
    }
}

/// An implementation of `StorageAsPointer` for any type that implements `StorageMemberAccessTrait`
/// and `Store`.
impl StorageMemberStateAsPointer<
    TMemberState,
    +StorageMemberAddressTrait<TMemberState>,
    +starknet::Store<StorageMemberAddressTrait::<TMemberState>::Value>,
> of StorageAsPointer<TMemberState, StorageMemberAddressTrait::<TMemberState>::Value> {
    fn as_ptr(
        self: @TMemberState
    ) -> StoragePointer<StorageMemberAddressTrait::<TMemberState>::Value> {
        StoragePointer::<
            StorageMemberAddressTrait::<TMemberState>::Value
        > { address: self.address() }
    }
}

/// Trait for updating the hash state with a value, using an `entry` method.
// TODO(Gil): Once associated types are stabilized, make `K` and `V` associated types of this trait.
pub trait StoragePathEntry<C, K, V> {
    fn entry(self: StoragePath<C>, key: K) -> StoragePath<V>;
}

/// A struct that represents a map in a contract storage.
#[phantom]
pub struct Map<K, V> {}

impl StoragePathEntryMap<
    K, V, +core::hash::Hash<K, core::poseidon::HashState>
> of StoragePathEntry<Map<K, V>, K, V> {
    #[inline(always)]
    fn entry(self: StoragePath<Map<K, V>>, key: K) -> StoragePath<V> {
        StoragePath::<
            V
        > {
            hash_state: core::hash::Hash::<
                K, core::poseidon::HashState
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


/// A struct for delaying the creation of a storage path, used for lazy evaluation in storage nodes.
#[derive(Copy, Drop)]
struct PendingStoragePath<T> {
    hash_state: core::poseidon::HashState,
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
        let address = self.address().into();
        StoragePath::<
            Self::Value
        > {
            hash_state: core::hash::HashStateTrait::update(
                core::poseidon::PoseidonTrait::new(), address
            )
        }
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
        StoragePath::<
            Self::Value
        > {
            hash_state: core::hash::HashStateTrait::update(
                core::poseidon::PoseidonTrait::new(), self.address().into()
            )
        }
    }
}

