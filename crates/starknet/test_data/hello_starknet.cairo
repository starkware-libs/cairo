#[contract]
mod HelloStarknet {
    struct Storage { balance: felt, }

    // Increases the balance by the given amount.
    #[external]
    // TODO(yuval): return nothing
    // TODO(yuval): make system implicit.
    fn increase_balance(ref system: System, amount: felt) {
        let res = super::balance::read(system);
        super::balance::write(system, res + amount);
    }

    // Returns the current balance.
    #[view]
    fn get_balance(ref system: System) -> felt {
        super::balance::read(system)
    }
}
