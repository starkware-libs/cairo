#[contract]
mod MinimalContract {
    #[starknet::storage]
    struct Storage {}
    #[external]
    fn empty(ref self: Storage) {}
}
