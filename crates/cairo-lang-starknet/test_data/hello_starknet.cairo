#[starknet::contract]
mod HelloStarknet {
    #[storage]
    struct Storage {
        balance: felt252, 
    }

    // Increases the balance by the given amount.
    #[external]
    fn increase_balance(ref self: ContractState, amount: felt252) {
        self.balance.write(self.balance.read() + amount);
    }

    // Returns the current balance.
    #[external]
    fn get_balance(self: @ContractState) -> felt252 {
        self.balance.read()
    }
}
