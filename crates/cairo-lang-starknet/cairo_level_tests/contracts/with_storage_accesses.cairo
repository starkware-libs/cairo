use starknet::ContractAddress;

#[starknet::contract]
mod storage_accesses_contract {
    use cairo_level_tests::components::storage_accesses::storage_accesses as storage_accesses_comp;
    use starknet::ContractAddress;
    #[storage]
    struct Storage {
        #[substorage(v0)]
        storage_accesses_token: storage_accesses_comp::Storage,
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        ERC20Token: storage_accesses_comp::Event,
    }

    component!(path: storage_accesses_comp, storage: storage_accesses_token, event: ERC20Token);

    #[abi(embed_v0)]
    impl ERC20Impl = storage_accesses_comp::IERC20<ContractState>;

    impl ERC20HelperImpl = storage_accesses_comp::ERC20HelperImpl<ContractState>;

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
            recipient: ContractAddress
        ) {
            self.storage_accesses_token.init(name, symbol, decimals, initial_supply, recipient);
        }
    }
}
