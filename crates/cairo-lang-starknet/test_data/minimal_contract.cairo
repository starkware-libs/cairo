#[starknet::contract]
mod MinimalContract {
    #[starknet::storage]
    struct Storage {}
    #[starknet::external]
    fn empty(ref self: Storage) {}
}
