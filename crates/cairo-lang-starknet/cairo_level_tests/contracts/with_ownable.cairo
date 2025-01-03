#[starknet::contract]
mod ownable_balance {
    use starknet::ContractAddress;
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use crate::components::ownable::ownable as ownable_comp;
    #[storage]
    struct Storage {
        #[substorage(v0)]
        ownable: ownable_comp::Storage,
        balance: u128,
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        Ownable: ownable_comp::Event,
    }

    component!(path: ownable_comp, storage: ownable, event: Ownable);

    #[abi(embed_v0)]
    impl OwnershipTransfer = ownable_comp::Transfer<ContractState>;

    impl OwnershipHelper = ownable_comp::OwnableHelperImpl<ContractState>;

    #[abi(per_item)]
    #[generate_trait]
    impl OwnableBalanceImpl of OwnableBalance {
        #[constructor]
        fn constructor(ref self: ContractState, owner: ContractAddress, initial: u128) {
            self.ownable.init_ownable(owner);
            self.balance.write(initial);
        }
        #[external(v0)]
        fn get_balance(self: @ContractState) -> u128 {
            self.balance.read()
        }
        #[external(v0)]
        fn set_balance(ref self: ContractState, new_balance: u128) {
            self.ownable.validate_ownership();
            self.balance.write(new_balance);
        }
    }
}
