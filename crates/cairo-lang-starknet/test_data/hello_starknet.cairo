#[contract]
mod HelloStarknet {
    struct Storage { balance: felt, }

    // Increases the balance by the given amount.
    #[external]
    fn increase_balance(amount: felt) {
        balance::write(balance::read() + amount);
    }

    // Returns the current balance.
    #[view]
    fn get_balance() -> felt {
        balance::read()
        0
    }
}
