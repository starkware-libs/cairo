#[starknet::component]
mod ownable {
    use starknet::ContractAddress;
    #[storage]
    struct Storage {
        owner: starknet::ContractAddress,
    }

    #[event]
    #[derive(Copy, Drop, starknet::Event)]
    struct Event {}

    #[embeddable_as(Transfer)]
    #[generate_trait]
    impl TransferImpl<
        TContractState, impl X: HasComponent<TContractState>
    > of TransferTrait<TContractState, X> {
        fn init_ownable(ref self: ComponentState<TContractState>, owner: ContractAddress) {
            self.owner.write(owner);
        }

        #[external(v0)]
        fn owner(self: @ComponentState<TContractState>) -> ContractAddress {
            self.owner.read()
        }

        #[external(v0)]
        fn transfer_ownership(
            ref self: ComponentState<TContractState>, new_owner: ContractAddress
        ) {
            self.validate_ownership();
            self.owner.write(new_owner);
        }

        fn validate_ownership(self: @ComponentState<TContractState>) {
            assert(self.owner.read() == starknet::get_caller_address(), 'Wrong owner.');
        }
    }
}

#[starknet::contract]
mod my_contract {
    use starknet::ContractAddress;
    #[storage]
    struct Storage {
        #[nested(v0)]
        ownable: super::ownable::Storage,
        balance: u128,
    }

    #[event]
    #[derive(Copy, Drop, starknet::Event)]
    enum Event {
        Ownable: super::ownable::Event,
    }

    component !(path: super::ownable, storage: ownable, event: Ownable);

    #[embed(v0)]
    impl OwnershipTransfer = super::ownable::Transfer<ContractState>;
    use super::ownable::TransferTrait;

    #[embed(v0)]
    #[generate_trait]
    impl Impl of Trait {
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
