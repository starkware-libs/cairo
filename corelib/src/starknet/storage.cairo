/// Trait for getting the address of any contract/component storage member.
trait StorageMemberAddressTrait<TMemberState, TValue> {
    fn address(self: @TMemberState) -> starknet::StorageBaseAddress nopanic;
}
/// Trait for accessing any contract/component storage member.
trait StorageMemberAccessTrait<TMemberState, TValue> {
    fn read(self: @TMemberState) -> TValue;
    fn write(ref self: TMemberState, value: TValue);
}
/// Implementation of StorageMemberAccessTrait for types that implement StorageMemberAddressTrait.
impl StorageMemberAccessImpl<
    TMemberState,
    TValue,
    +StorageMemberAddressTrait<TMemberState, TValue>,
    +starknet::Store<TValue>,
    +Drop<TMemberState>,
> of StorageMemberAccessTrait<TMemberState, TValue> {
    fn read(self: @TMemberState) -> TValue {
        // Only address_domain 0 is currently supported.
        let address_domain = 0_u32;
        starknet::SyscallResultTraitImpl::unwrap_syscall(
            starknet::Store::<TValue>::read(address_domain, self.address())
        )
    }
    fn write(ref self: TMemberState, value: TValue) {
        // Only address_domain 0 is currently supported.
        let address_domain = 0_u32;
        let write_result = starknet::Store::<TValue>::write(address_domain, self.address(), value);
        starknet::SyscallResultTraitImpl::unwrap_syscall(write_result)
    }
}

/// Trait for getting the address of any contract/component mapping storage member.
trait StorageMapMemberAddressTrait<TMemberState, TKey, TValue> {
    fn address(self: @TMemberState, key: TKey) -> starknet::StorageBaseAddress;
}
/// Trait for accessing any contract/component storage member.
trait StorageMapMemberAccessTrait<TMemberState, TKey, TValue> {
    fn read(self: @TMemberState, key: TKey) -> TValue;
    fn write(ref self: TMemberState, key: TKey, value: TValue);
}
/// Implementation of StorageMapMemberAccessTrait for types that implement
/// StorageMapMemberAddressTrait.
impl StorageMapMemberAccessImpl<
    TMemberState,
    TKey,
    TValue,
    +StorageMapMemberAddressTrait<TMemberState, TKey, TValue>,
    +starknet::Store<TValue>,
    +Drop<TMemberState>,
    +hash::LegacyHash<TKey>,
    +PanicDestruct<TValue>,
> of StorageMapMemberAccessTrait<TMemberState, TKey, TValue> {
    fn read(self: @TMemberState, key: TKey) -> TValue {
        // Only address_domain 0 is currently supported.
        let address_domain = 0_u32;
        starknet::SyscallResultTraitImpl::unwrap_syscall(
            starknet::Store::<TValue>::read(address_domain, self.address(key))
        )
    }
    fn write(ref self: TMemberState, key: TKey, value: TValue) {
        // Only address_domain 0 is currently supported.
        let address_domain = 0_u32;
        starknet::SyscallResultTraitImpl::unwrap_syscall(
            starknet::Store::<TValue>::write(address_domain, self.address(key), value)
        )
    }
}

// TODO(v3): remove this module, it's not needed and only here to not break existing explicit usages
// of the old trait.
mod old {
    trait StorageMemberStateTrait<TMemberState, TValue> {
        fn address(state: @TMemberState) -> starknet::StorageBaseAddress;
        fn read(state: @TMemberState) -> TValue;
        fn write(ref state: TMemberState, value: TValue);
    }
    impl StorageMemberStateImpl<
        TMemberState,
        TValue,
        impl StorageMemberAddressImpl: super::StorageMemberAddressTrait<TMemberState, TValue>,
        impl StorageMemberAccessImpl: super::StorageMemberAccessTrait<TMemberState, TValue>
    > of StorageMemberStateTrait<TMemberState, TValue> {
        fn address(state: @TMemberState) -> starknet::StorageBaseAddress {
            StorageMemberAddressImpl::address(state)
        }
        fn read(state: @TMemberState) -> TValue {
            StorageMemberAccessImpl::read(state)
        }
        fn write(ref state: TMemberState, value: TValue) {
            StorageMemberAccessImpl::write(ref state, value)
        }
    }
    trait StorageMapMemberStateTrait<TMemberState, TKey, TValue> {
        fn address(state: @TMemberState, key: TKey) -> starknet::StorageBaseAddress;
        fn read(state: @TMemberState, key: TKey) -> TValue;
        fn write(ref state: TMemberState, key: TKey, value: TValue);
    }
    impl StorageMapMemberStateImpl<
        TMemberState,
        TKey,
        TValue,
        impl StorageMapMemberAddressImpl: super::StorageMapMemberAddressTrait<TMemberState,
        TKey,
        TValue>,
        impl StorageMapMemberAccessImpl: super::StorageMapMemberAccessTrait<TMemberState,
        TKey,
        TValue>
    > of StorageMapMemberStateTrait<TMemberState, TKey, TValue> {
        fn address(state: @TMemberState, key: TKey) -> starknet::StorageBaseAddress {
            StorageMapMemberAddressImpl::address(state, key)
        }
        fn read(state: @TMemberState, key: TKey) -> TValue {
            StorageMapMemberAccessImpl::read(state, key)
        }
        fn write(ref state: TMemberState, key: TKey, value: TValue) {
            StorageMapMemberAccessImpl::write(ref state, key, value)
        }
    }
}
