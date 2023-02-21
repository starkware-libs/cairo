#[abi]
trait IAnotherContract {
    fn foo(a: u128) -> u128;
}


#[account_contract]
mod Account {
    struct Storage {
        public_key: felt
    }
// TODO(ilya): Implement account contract functions.
}
