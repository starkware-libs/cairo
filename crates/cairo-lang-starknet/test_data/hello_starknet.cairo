#[contract]
mod HelloStarknet {
    #[starknet::storage]
    struct Storage {
        balance: felt252, 
    }

    // Increases the balance by the given amount.
    #[external]
    fn increase_balance(ref self: Storage, amount: felt252) {
        self.balance.write(self.balance.read() + amount);
    }

    // Returns the current balance.
    #[external]
    fn get_balance(self: @Storage) -> felt252 {
        self.balance.read()
    }
}
