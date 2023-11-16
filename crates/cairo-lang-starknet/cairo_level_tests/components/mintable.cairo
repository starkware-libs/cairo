use starknet::ContractAddress;

#[starknet::interface]
pub trait MintTrait<TContractState> {
    fn mint(ref self: TContractState, account: ContractAddress, amount: u256);
}

#[starknet::component]
pub mod mintable {
    use core::num::traits::Zero;
    use starknet::{ContractAddress, contract_address_const};
    use cairo_level_tests::components::erc20::erc20 as erc20_comp;
    use cairo_level_tests::components::ownable::ownable as ownable_comp;
    use ownable_comp::OwnableHelperImpl;

    #[storage]
    struct Storage {}

    #[embeddable_as(Mint)]
    pub impl MintImpl<
        TContractState,
        +HasComponent<TContractState>,
        impl Ownable: ownable_comp::HasComponent<TContractState>,
        impl ERC20: erc20_comp::HasComponent<TContractState>,
        +Drop<TContractState>
    > of super::MintTrait<ComponentState<TContractState>> {
        fn mint(ref self: ComponentState<TContractState>, account: ContractAddress, amount: u256) {
            assert(!account.is_zero(), 'ERC20: mint to the 0 address');
            get_dep_component!(self, Ownable).validate_ownership();
            let mut erc20_component = get_dep_component_mut!(ref self, ERC20);
            let total_supply = erc20_component.total_supply.read();
            erc20_component.total_supply.write(total_supply + amount);
            erc20_component
                .balances
                .write(account, erc20_component.balances.read(account) + amount);
            erc20_component
                .emit(
                    erc20_comp::TransferEvent {
                        from: contract_address_const::<0>(), to: account, value: amount
                    }
                );
        }
    }
}
