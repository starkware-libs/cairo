use starknet::ContractAddress;

#[starknet::interface]
trait FirstInterface<T> {
    fn toto(self: @T, t: Array<ContractAddress>);
    fn balance_of(self: @T);
}


#[starknet::contract]
mod toto {
    #[storage]
    struct Storage {}

    use super::{FirstInterfaceDispatcher, FirstInterfaceDispatcherTrait};

    #[external(v0)]
    fn test_mint_and_unbox(self: @ContractState) {
        let contract = FirstInterfaceDispatcher {
            contract_address: starknet::contract_address_const::<5>()
        };

        contract.balance_of();
        contract.toto(array![contract.contract_address]);
        contract.balance_of();
    }
}
