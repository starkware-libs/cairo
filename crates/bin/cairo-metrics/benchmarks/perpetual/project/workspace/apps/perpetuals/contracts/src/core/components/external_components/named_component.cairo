#[starknet::interface]
pub trait ITypedComponent<TContractState> {
    fn component_type(ref self: TContractState) -> felt252;
}

