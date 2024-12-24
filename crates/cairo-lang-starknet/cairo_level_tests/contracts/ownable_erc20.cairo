#[starknet::contract]
mod ownable_erc20_contract {
    use starknet::ContractAddress;
    use crate::components::erc20::erc20 as erc20_comp;
    use crate::components::ownable::ownable as ownable_comp;
    #[storage]
    struct Storage {
        #[substorage(v0)]
        ownable_storage: ownable_comp::Storage,
        #[substorage(v0)]
        erc20_storage: erc20_comp::Storage,
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        ERC20: erc20_comp::Event,
        Ownable: ownable_comp::Event,
    }

    component!(path: erc20_comp, storage: erc20_storage, event: ERC20);
    component!(path: ownable_comp, storage: ownable_storage, event: Ownable);

    #[abi(embed_v0)]
    impl ERC20Impl = erc20_comp::IERC20<ContractState>;
    #[abi(embed_v0)]
    impl OwnershipTransfer = ownable_comp::Transfer<ContractState>;


    impl ERC20HelperImpl = erc20_comp::ERC20HelperImpl<ContractState>;
    impl OwnershipHelper = ownable_comp::OwnableHelperImpl<ContractState>;


    #[abi(per_item)]
    #[generate_trait]
    impl OwnableERC20Impl of OwnableERC20 {
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
}
