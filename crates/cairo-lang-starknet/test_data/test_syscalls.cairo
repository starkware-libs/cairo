#[contract]
mod SyscallTester {
    use starknet::ContractAddress;

    #[external]
    fn get_caller_address() -> ContractAddress {
        starknet::get_caller_address()
    }

    #[external]
    fn get_contract_address() -> ContractAddress {
        starknet::get_contract_address()
    }
}
