#[starknet::contract]
mod erc20_contract {
    use starknet::ContractAddress;
    use crate::components::erc20::erc20 as erc20_comp;
    #[storage]
    struct Storage {
        #[substorage(v0)]
        erc20_token: erc20_comp::Storage,
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        ERC20Token: erc20_comp::Event,
    }

    component!(path: erc20_comp, storage: erc20_token, event: ERC20Token);

    #[abi(embed_v0)]
    impl ERC20Impl = erc20_comp::IERC20<ContractState>;

    impl ERC20HelperImpl = erc20_comp::ERC20HelperImpl<ContractState>;

    #[abi(per_item)]
    #[generate_trait]
    impl CtorImpl of CtorTrait {
        #[constructor]
        fn constructor(
            ref self: ContractState,
            name: felt252,
            symbol: felt252,
            decimals: u8,
            initial_supply: u256,
            recipient: ContractAddress,
        ) {
            self.erc20_token.init(name, symbol, decimals, initial_supply, recipient);
        }
    }
}
