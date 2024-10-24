#[starknet::interface]
trait Interface1<TContractState> {
    fn foo(ref self: TContractState);
}

#[starknet::interface]
trait Interface2<TContractState> {
    fn foo(ref self: TContractState);
}

#[starknet::component]
mod comp {
    #[storage]
    struct Storage {}

    #[embeddable_as(I1I1)]
    impl Interface1_Implementation1<
        TContractState, impl X: HasComponent<TContractState>
    > of super::Interface1<ComponentState<TContractState>> {
        fn foo(ref self: ComponentState<TContractState>) {}
    }

    #[embeddable_as(I1I2)]
    impl Interface1_Implementation2<
        TContractState, impl X: HasComponent<TContractState>
    > of super::Interface1<ComponentState<TContractState>> {
        fn foo(ref self: ComponentState<TContractState>) {}
    }

    #[embeddable_as(I2I)]
    impl Interface2_Implementation<
        TContractState, impl X: HasComponent<TContractState>
    > of super::Interface2<ComponentState<TContractState>> {
        fn foo(ref self: ComponentState<TContractState>) {}
    }
}

#[starknet::contract]
mod counter_contract {
    component!(path: super::comp, storage: comp_storage, event: CompEvent);

    #[storage]
    struct Storage {
        #[substorage(v0)]
        comp_storage: super::comp::Storage
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        CompEvent: super::comp::Event
    }

    #[abi(embed_v0)]
    impl EmbeddedI1I1 = super::comp::I1I1<ContractState>;
    #[abi(embed_v0)]
    impl EmbeddedI1I2 = super::comp::I1I2<ContractState>;
    #[abi(embed_v0)]
    impl EmbeddedI2I = super::comp::I2I<ContractState>;

    #[external(v0)]
    fn foo(self: @ContractState) {}
}
