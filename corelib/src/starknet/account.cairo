use starknet::ContractAddress;

/// A struct representing a call to a contract.
#[derive(Drop, Copy, Serde, Debug)]
pub struct Call {
    /// The address of the contract to call.
    pub to: ContractAddress,
    /// The entry point selector in the called contract.
    pub selector: felt252,
    /// The calldata to pass to entry point.
    pub calldata: Span<felt252>
}

/// A trait for account contracts that support class declarations (only validate and execute are
/// mandatory for an account).
///
/// This trait assumes that the calldata for invoke transactions is `Array<Call>`.
/// This is the network standard following SNIP6.
/// It is not enforced by StarkNet, but deviating from the standard interface may lead to
/// incompatibility with standard tooling.
#[starknet::interface]
pub trait AccountContract<TContractState> {
    /// An entry point that is called to check if the account is willing to pay for the declaration
    /// of the class with the given hash.
    /// The entry point should return `core::starknet::VALIDATED` if the account is willing to pay
    /// for the declaration.
    fn __validate_declare__(self: @TContractState, class_hash: felt252) -> felt252;

    /// An entry point that is called to check if the account is willing to pay for
    /// executing a given set of calls.
    /// The entry point should return `core::starknet::VALIDATED` if the account is willing to pay
    /// for the execution, in which case `__execute__` will be called on the same set of calls.
    fn __validate__(ref self: TContractState, calls: Array<Call>) -> felt252;

    /// An entry point that is called to execute a given set of calls.
    /// This entry point should block v0 transactions as they do not go through the `__validate__`
    /// entry point.
    fn __execute__(ref self: TContractState, calls: Array<Call>) -> Array<Span<felt252>>;
}
