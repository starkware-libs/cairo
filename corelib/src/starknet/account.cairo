use starknet::ContractAddress;

#[derive(Drop, Serde)]
struct Call {
    to: ContractAddress,
    selector: felt252,
    calldata: Array<felt252>
}

trait AccountContract<TContractState> {
    fn __validate_declare__(self: @TContractState, class_hash: felt252) -> felt252;
    fn __validate__(
        ref self: TContractState,
        contract_address: ContractAddress,
        entry_point_selector: felt252,
        calldata: Array<felt252>
    ) -> felt252;
    fn __execute__(ref self: TContractState, calls: Array<Call>) -> Span<felt252>;
}
