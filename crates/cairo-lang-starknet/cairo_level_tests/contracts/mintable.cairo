#[starknet::interface]
trait GetSupply<TContractState> {
    fn get_total_supply_plus_1(self: @TContractState) -> u256;
}

#[starknet::contract]
mod mintable_erc20_ownable {
    use starknet::ContractAddress;
    use crate::components::erc20::erc20 as erc20_comp;
    use crate::components::mintable::mintable as mintable_comp;
    use crate::components::ownable::ownable as ownable_comp;
    #[storage]
    struct Storage {
        #[substorage(v0)]
        erc20_storage: erc20_comp::Storage,
        #[substorage(v0)]
        ownable_storage: ownable_comp::Storage,
        #[substorage(v0)]
        mintable_storage: mintable_comp::Storage,
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        ERC20: erc20_comp::Event,
        Ownable: ownable_comp::Event,
        Mintable: mintable_comp::Event,
    }

    component!(path: erc20_comp, storage: erc20_storage, event: ERC20);
    component!(path: ownable_comp, storage: ownable_storage, event: Ownable);
    component!(path: mintable_comp, storage: mintable_storage, event: Mintable);

    #[abi(embed_v0)]
    impl ERC20Impl = erc20_comp::IERC20<ContractState>;

    impl ERC20Helper = erc20_comp::ERC20HelperImpl<ContractState>;

    #[abi(embed_v0)]
    impl OwnershipTransfer = ownable_comp::Transfer<ContractState>;

    impl OwnershipHelper = ownable_comp::OwnableHelperImpl<ContractState>;

    #[abi(embed_v0)]
    impl MintImpl = mintable_comp::Mint<ContractState>;

    #[abi(per_item)]
    #[generate_trait]
    impl ImplCtor of TraitCtor {
        #[constructor]
        fn constructor(
            ref self: ContractState,
            name: felt252,
            symbol: felt252,
            decimals: u8,
            initial_supply: u256,
            recipient: ContractAddress,
            owner: ContractAddress,
        ) {
            self.erc20_storage.init(name, symbol, decimals, initial_supply, recipient);
            self.ownable_storage.init_ownable(owner);
        }
    }

    #[abi(embed_v0)]
    impl ImplGetSupply of super::GetSupply<ContractState> {
        fn get_total_supply_plus_1(self: @ContractState) -> u256 {
            self.erc20_storage.get_total_supply() + 1
        }
    }
}
