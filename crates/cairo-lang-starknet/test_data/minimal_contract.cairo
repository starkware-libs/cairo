#[starknet::contract]
mod MinimalContract {
    #[storage]
    struct Storage {}
    #[external]
    fn empty(ref self: ContractState) {}
}
