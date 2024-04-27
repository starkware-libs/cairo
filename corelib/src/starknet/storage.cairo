use core::poseidon::HashState;
use starknet::storage_access::StorageBaseAddress;
use starknet::SyscallResult;
use starknet::storage_access::storage_base_address_from_felt252;

/// Trait for getting the address of any contract/component storage member.
pub trait StorageMemberAddressTrait<TMemberState, TValue> {
    fn address(self: @TMemberState) -> starknet::StorageBaseAddress nopanic;
}

/// Trait for accessing any contract/component storage member.
pub trait StorageMemberAccessTrait<TMemberState, TValue> {
    fn read(self: @TMemberState) -> TValue;
    fn write(ref self: TMemberState, value: TValue);
}

/// Implementation of StorageMemberAccessTrait for types that implement StorageMemberAddressTrait.
pub impl StorageMemberAccessImpl<
    TMemberState,
    TValue,
    +StorageMemberAddressTrait<TMemberState, TValue>,
    +starknet::Store<TValue>,
    +Drop<TMemberState>,
> of StorageMemberAccessTrait<TMemberState, TValue> {
    fn read(self: @TMemberState) -> TValue {
        // Only address_domain 0 is currently supported.
        let address_domain = 0_u32;
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<TValue>::read(address_domain, self.address())
        )
    }
    fn write(ref self: TMemberState, value: TValue) {
        // Only address_domain 0 is currently supported.
        let address_domain = 0_u32;
        let write_result = starknet::Store::<TValue>::write(address_domain, self.address(), value);
        starknet::SyscallResultTrait::unwrap_syscall(write_result)
    }
}

/// Trait for getting the address of any contract/component mapping storage member.
pub trait StorageMapMemberAddressTrait<TMemberState, TKey, TValue> {
    fn address(self: @TMemberState, key: TKey) -> starknet::StorageBaseAddress;
}

/// Trait for accessing any contract/component storage member.
pub trait StorageMapMemberAccessTrait<TMemberState, TKey, TValue> {
    fn read(self: @TMemberState, key: TKey) -> TValue;
    fn write(ref self: TMemberState, key: TKey, value: TValue);
}

/// Implementation of StorageMapMemberAccessTrait for types that implement
/// StorageMapMemberAddressTrait.
pub impl StorageMapMemberAccessImpl<
    TMemberState,
    TKey,
    TValue,
    +StorageMapMemberAddressTrait<TMemberState, TKey, TValue>,
    +starknet::Store<TValue>,
    +Drop<TMemberState>,
    +PanicDestruct<TValue>,
> of StorageMapMemberAccessTrait<TMemberState, TKey, TValue> {
    fn read(self: @TMemberState, key: TKey) -> TValue {
        // Only address_domain 0 is currently supported.
        let address_domain = 0_u32;
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<TValue>::read(address_domain, self.address(key))
        )
    }
    fn write(ref self: TMemberState, key: TKey, value: TValue) {
        // Only address_domain 0 is currently supported.
        let address_domain = 0_u32;
        starknet::SyscallResultTrait::unwrap_syscall(
            starknet::Store::<TValue>::write(address_domain, self.address(key), value)
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
pub trait StorageAsPath<TMemberState, T> {
    fn as_path(self: @TMemberState) -> StoragePath<T>;
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
