#[starknet::contract]
mod erc20_mini_contract {
    use starknet::ContractAddress;
    use starknet::storage::{StorageTrait, StorageTraitMut};
    use crate::components::erc20_mini;
    #[storage]
    struct Storage {
        #[flat]
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
        fn storage(self: @ContractState) -> StorageTrait::<erc20_mini::ERC20Storage>::BaseType {
            self.erc20_token.deref()
        }
        fn storage_mut(
            ref self: ContractState,
        ) -> StorageTraitMut::<erc20_mini::ERC20Storage>::BaseType {
            self.erc20_token.deref()
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
            recipient: ContractAddress,
        ) {
            self.erc20_init(name, symbol, decimals, initial_supply, recipient);
        }
    }
}
