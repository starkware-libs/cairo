#[starknet::contract]
mod minimal_contract {
    #[storage]
    struct Storage {}
    #[external(v0)]
    fn empty(ref self: ContractState) {}
}
