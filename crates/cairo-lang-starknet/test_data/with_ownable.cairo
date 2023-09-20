use starknet::ContractAddress;

#[starknet::interface]
trait TransferTrait<TCS> {
    fn owner(self: @TCS) -> ContractAddress;
    fn transfer_ownership(ref self: TCS, new_owner: ContractAddress);
}

#[starknet::component]
mod ownable {
    use starknet::ContractAddress;
    #[storage]
    struct Storage {
        owner: ContractAddress,
    }

    #[embeddable_as(Transfer)]
    impl TransferImpl<
        TContractState, impl X: HasComponent<TContractState>
    > of super::TransferTrait<ComponentState<TContractState>> {
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
    }

    #[generate_trait]
    impl OwnableHelperImpl<
        TContractState, impl X: HasComponent<TContractState>
    > of OwnableHelperTrait<TContractState, X> {
        fn init_ownable(ref self: ComponentState<TContractState>, owner: ContractAddress) {
            self.owner.write(owner);
        }
        fn validate_ownership(self: @ComponentState<TContractState>) {
            assert(self.owner.read() == starknet::get_caller_address(), 'Wrong owner.');
        }
    }
}

#[starknet::contract]
mod ownable_balance {
    use starknet::ContractAddress;
    #[storage]
    struct Storage {
        #[substorage(v0)]
        ownable: super::ownable::Storage,
        balance: u128,
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        Ownable: super::ownable::Event,
    }

    component!(path: super::ownable, storage: ownable, event: Ownable);

    #[abi(embed_v0)]
    impl OwnershipTransfer = super::ownable::Transfer<ContractState>;

    impl OwnershipHelper = super::ownable::OwnableHelperImpl<ContractState>;

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
