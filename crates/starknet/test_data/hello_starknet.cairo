#[contract]
mod HelloStarknet {
    struct Storage { balance: felt, }

    // Increases the balance by the given amount.
    #[external]
    // TODO(yuval): return nothing
    // TODO(yuval): make system implicit.
    func increase_balance(ref system: System, amount: felt) -> felt {
        let res = super::balance::read(system);
        super::balance::write(system, res + amount);
        0
    }

    // Returns the current balance.
    // TODO(yuval): change to #[view]
    #[external]
    func get_balance(ref system: System) -> felt {
        super::balance::read(system)
    }
}
