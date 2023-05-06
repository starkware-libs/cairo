#[contract]
mod MinimalContract {
    #[starknet::storage]
    struct Storage {}
    #[derive(starknet::Event)]
    enum Event {}
    #[external]
    fn empty(ref self: Storage) {}
}
