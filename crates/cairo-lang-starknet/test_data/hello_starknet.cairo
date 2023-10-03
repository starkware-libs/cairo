#[starknet::interface]
trait HelloStarknetTrait<TContractState> {
    // Increases the balance by the given amount.
    fn increase_balance(ref self: TContractState, amount: usize);
    // Returns the current balance.
    fn get_balance(self: @TContractState) -> usize;
}

#[starknet::contract]
mod hello_starknet {
    #[storage]
    struct Storage {
        balance: usize,
    }

    #[abi(embed_v0)]
    impl HelloStarknetImpl of super::HelloStarknetTrait<ContractState> {
        fn increase_balance(ref self: ContractState, amount: usize) {
            self.balance.write(self.balance.read() + amount);
        }
        fn get_balance(self: @ContractState) -> usize {
            self.balance.read()
        }
    }
}

