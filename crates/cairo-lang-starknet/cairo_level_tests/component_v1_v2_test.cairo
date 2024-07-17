use core::starknet::storage::{StoragePointerWriteAccess, StoragePointerReadAccess};
#[starknet::component]
mod comp_v1 {
    #[storage]
    pub struct Storage {
        pub member: felt252,
    }
}

#[starknet::contract]
mod contract_v1 {
    use super::comp_v1;
    component!(path: comp_v1, storage: comp, event: comp);
    #[storage]
    pub struct Storage {
        #[substorage(v0)]
        pub comp: comp_v1::Storage,
    }
    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        comp: comp_v1::Event,
    }
}

/// A trait that, given a path to a storage, provides the storage node and mutable storage node.
pub trait HasStorage<
    TContractState,
    /// The storage type.
    Storage,
    /// The storage node.
    impl StorageImpl: starknet::storage::StorageTrait<Storage>,
    /// The mutable storage node.
    impl StorageImplMut: starknet::storage::StorageTraitMut<Storage>
> {
    fn storage(self: @TContractState) -> StorageImpl::BaseType;
    fn storage_mut(ref self: TContractState) -> StorageImplMut::BaseType;
}

#[starknet::storage_node]
struct ComponentV2Storage {
    member: felt252,
}

#[starknet::interface]
pub trait ComponentV2Trait<TCS> {
    fn get_member(self: @TCS) -> felt252;
    fn set_member(ref self: TCS, value: felt252);
}

#[starknet::embeddable]
pub impl ComponentV2Impl<
    TContractState, +HasStorage<TContractState, ComponentV2Storage>, +Drop<TContractState>
> of ComponentV2Trait<TContractState> {
    fn get_member(self: @TContractState) -> felt252 {
        self.storage().member.read()
    }
    fn set_member(ref self: TContractState, value: felt252) {
        self.storage_mut().member.write(value);
    }
}


#[starknet::contract]
mod contract_v2 {
    use starknet::storage::{StorageTrait, StorageTraitMut};
    use super::ComponentV2Storage;
    #[storage]
    pub struct Storage {
        #[flat]
        pub comp: ComponentV2Storage,
    }
    #[abi(embed_v0)]
    impl ERC20Impl = super::ComponentV2Impl<ContractState>;

    impl ComponentV2Impl of super::HasStorage<ContractState, ComponentV2Storage> {
        fn storage(self: @ContractState) -> StorageTrait::<ComponentV2Storage>::BaseType {
            self.comp.deref()
        }
        fn storage_mut(ref self: ContractState) -> StorageTraitMut::<ComponentV2Storage>::BaseType {
            self.comp.deref()
        }
    }
}

#[test]
fn contract_v1_write_v2_read_test() {
    let mut contract_state_v1 = contract_v1::contract_state_for_testing();
    let mut contract_state_v2 = contract_v2::contract_state_for_testing();
    contract_state_v1.comp.member.write(1);
    assert_eq!(contract_state_v2.get_member(), 1);
}

#[test]
fn contract_v2_write_v1_read_test() {
    let mut contract_state_v1 = contract_v1::contract_state_for_testing();
    let mut contract_state_v2 = contract_v2::contract_state_for_testing();
    contract_state_v2.set_member(1);
    assert_eq!(contract_state_v1.comp.member.read(), 1);
}
