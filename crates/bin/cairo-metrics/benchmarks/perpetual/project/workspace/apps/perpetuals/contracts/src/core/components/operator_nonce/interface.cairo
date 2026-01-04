#[starknet::interface]
pub trait IOperatorNonce<TContractState> {
    fn get_operator_nonce(self: @TContractState) -> u64;
}
