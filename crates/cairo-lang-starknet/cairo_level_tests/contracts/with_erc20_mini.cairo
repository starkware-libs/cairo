use starknet::ContractAddress;

#[starknet::contract]
mod erc20_mini_contract {
    use cairo_level_tests::components::erc20_mini;
    use starknet::ContractAddress;
    use starknet::storage::{StorageAsPath, StorageNode, StorageNodeMut};
    #[storage]
    struct Storage {
        erc20_token: erc20_mini::ERC20Storage,
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        ERC20Token: erc20_mini::Event,
    }

    #[abi(embed_v0)]
    impl ERC20Impl = erc20_mini::ERC20Impl<ContractState, Event>;

    impl ERC20HelperImpl = erc20_mini::ERC20HelperImpl<ContractState, Event>;

    impl ERC20HasStorage of erc20_mini::HasStorage<ContractState, erc20_mini::ERC20Storage> {
        fn storage(self: @ContractState) -> StorageNode::<erc20_mini::ERC20Storage>::NodeType {
            self.erc20_token.as_path().storage_node()
        }
        fn storage_mut(
            ref self: ContractState
        ) -> StorageNodeMut::<erc20_mini::ERC20Storage>::NodeType {
            self.erc20_token.as_path().storage_node_mut()
        }
    }

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
            self.erc20_init(name, symbol, decimals, initial_supply, recipient);
        }
    }
}
