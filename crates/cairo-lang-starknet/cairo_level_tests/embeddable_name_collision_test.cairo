#[starknet::interface]
trait IFirst<T> {
    fn first(self: @T) -> felt252;
}

#[starknet::interface]
trait ISecond<T> {
    fn second(self: @T) -> felt252;
}

#[starknet::component]
mod first_comp {
    #[storage]
    pub struct Storage {}

    #[embeddable_as(ExternalImpl)]
    impl FirstImpl<
        TContractState, +HasComponent<TContractState>,
    > of super::IFirst<ComponentState<TContractState>> {
        fn first(self: @ComponentState<TContractState>) -> felt252 {
            1
        }
    }
}

#[starknet::component]
mod second_comp {
    #[storage]
    pub struct Storage {}

    #[embeddable_as(ExternalImpl)]
    impl SecondImpl<
        TContractState, +HasComponent<TContractState>,
    > of super::ISecond<ComponentState<TContractState>> {
        fn second(self: @ComponentState<TContractState>) -> felt252 {
            2
        }
    }
}

/// Embeds two impls that have the same name in different components.
#[starknet::contract]
mod contract_with_same_named_embeddables {
    component!(path: super::first_comp, storage: first, event: FirstEvent);
    component!(path: super::second_comp, storage: second, event: SecondEvent);

    #[abi(embed_v0)]
    impl FirstExternal = super::first_comp::ExternalImpl<ContractState>;
    #[abi(embed_v0)]
    impl SecondExternal = super::second_comp::ExternalImpl<ContractState>;

    #[storage]
    struct Storage {
        #[substorage(v0)]
        first: super::first_comp::Storage,
        #[substorage(v0)]
        second: super::second_comp::Storage,
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        FirstEvent: super::first_comp::Event,
        SecondEvent: super::second_comp::Event,
    }
}

#[test]
fn test_same_named_embeddables() {
    let (contract_address, _) = starknet::syscalls::deploy_syscall(
        contract_with_same_named_embeddables::TEST_CLASS_HASH, 0, [].span(), false,
    )
        .unwrap();
    assert_eq!(IFirstDispatcher { contract_address }.first(), 1);
    assert_eq!(ISecondDispatcher { contract_address }.second(), 2);
}
