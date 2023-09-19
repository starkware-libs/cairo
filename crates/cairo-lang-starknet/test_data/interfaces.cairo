#[starknet::interface]
trait Interface1<TCS> {
    fn foo(ref self: TCS);
}

#[starknet::interface]
trait Interface2<TCS> {
    fn foo(ref self: TCS);
}

#[starknet::component]
mod comp {
    #[storage]
    struct Storage {}

    #[embeddable_as(I1I)]
    impl Interface1_Implementation<
        TContractState, impl X: HasComponent<TContractState>
    > of super::Interface1<ComponentState<TContractState>> {
        #[external(v0)]
        fn foo(ref self: ComponentState<TContractState>) {}
    }

    #[embeddable_as(I2I)]
    impl Interface2_Implementation<
        TContractState, impl X: HasComponent<TContractState>
    > of super::Interface2<ComponentState<TContractState>> {
        #[external(v0)]
        fn foo(ref self: ComponentState<TContractState>) {}
    }
}

#[starknet::contract]
mod counter_contract {
    component!(path: super::comp, storage: comp_storage, event: CompEvent);

    #[storage]
    struct Storage {
        #[nested(v0)]
        comp_storage: super::comp::Storage
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        CompEvent: super::comp::Event
    }

    #[embed(v0)]
    impl EmbeddedI1I = super::comp::I1I<ContractState>;
    #[embed(v0)]
    impl EmbeddedI2I = super::comp::I2I<ContractState>;

    #[external(v0)]
    fn foo(self: @ContractState) {}
}
