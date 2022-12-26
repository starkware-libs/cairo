#[contract]
mod HelloStarknet {
    struct Storage { balance: felt, }

    // Increases the balance by the given amount.
    #[external]
    fn increase_balance(amount: felt) {
        super::balance::write(super::balance::read() + amount);
    }

    // Returns the current balance.
    #[view]
    fn get_balance() -> felt {
        super::balance::read()
    }
}
