use core::starknet::storage::{StoragePointerWriteAccess, StoragePointerReadAccess};

#[starknet::interface]
pub trait ComponentTrait<TCS> {
    fn get_member(self: @TCS) -> felt252;
    fn set_member(ref self: TCS, value: felt252);
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

#[starknet::component]
mod comp_v0 {
    use core::starknet::storage::{StoragePointerWriteAccess, StoragePointerReadAccess};
    #[storage]
    pub struct Storage {
        pub member: felt252,
    }

    #[embeddable_as(ICompV0)]
    pub impl ComponentV0Impl<
        TContractState, +HasComponent<TContractState>
    > of super::ComponentTrait<ComponentState<TContractState>> {
        fn get_member(self: @ComponentState<TContractState>) -> felt252 {
            self.member.read()
        }

        fn set_member(ref self: ComponentState<TContractState>, value: felt252) {
            self.member.write(value);
        }
    }
}

#[starknet::contract]
mod contract_v0 {
    use core::starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use super::comp_v0;
    component!(path: comp_v0, storage: comp, event: comp);
    #[storage]
    pub struct Storage {
        #[substorage(v0)]
        pub comp: comp_v0::Storage,
    }
    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        comp: comp_v0::Event,
    }
    #[abi(embed_v0)]
    impl CompV0Impl = comp_v0::ICompV0<ContractState>;
}

#[starknet::storage_node]
struct ComponentNewStorage {
    member: felt252,
}

#[starknet::embeddable]
pub impl ComponentNewImpl<
    TContractState, +HasStorage<TContractState, ComponentNewStorage>, +Drop<TContractState>
> of ComponentTrait<TContractState> {
    fn get_member(self: @TContractState) -> felt252 {
        self.storage().member.read()
    }
    fn set_member(ref self: TContractState, value: felt252) {
        self.storage_mut().member.write(value);
    }
}


#[starknet::contract]
mod contract_new {
    use starknet::storage::{StorageTrait, StorageTraitMut};
    use super::ComponentNewStorage;
    #[storage]
    pub struct Storage {
        #[flat]
        pub comp: ComponentNewStorage,
    }
    #[abi(embed_v0)]
    impl ERC20Impl = super::ComponentNewImpl<ContractState>;

    impl ComponentNewImpl of super::HasStorage<ContractState, ComponentNewStorage> {
        fn storage(self: @ContractState) -> StorageTrait::<ComponentNewStorage>::BaseType {
            self.comp.deref()
        }
        fn storage_mut(
            ref self: ContractState
        ) -> StorageTraitMut::<ComponentNewStorage>::BaseType {
            self.comp.deref()
        }
    }
}

#[test]
fn contract_v0_write_new_read_test() {
    let mut contract_state_v0 = contract_v0::contract_state_for_testing();
    let mut contract_state_new = contract_new::contract_state_for_testing();
    contract_state_v0.set_member(1);
    assert_eq!(contract_state_new.get_member(), 1);
}

#[test]
fn contract_new_write_v0_read_test() {
    let mut contract_state_v0 = contract_v0::contract_state_for_testing();
    let mut contract_state_new = contract_new::contract_state_for_testing();
    contract_state_new.set_member(2);
    assert_eq!(contract_state_v0.get_member(), 2);
}
