use openzeppelin_account::extensions::src9::interface::OutsideExecution;
use openzeppelin_account::interface::EthPublicKey;
use starknet::ClassHash;
use starknet::account::Call;

#[starknet::interface]
pub trait EthAccountUpgradeableABI<TState> {
    // ISRC6
    fn __execute__(self: @TState, calls: Array<Call>);
    fn __validate__(self: @TState, calls: Array<Call>) -> felt252;
    fn is_valid_signature(self: @TState, hash: felt252, signature: Array<felt252>) -> felt252;

    // ISRC5
    fn supports_interface(self: @TState, interface_id: felt252) -> bool;

    // ISRC9
    fn execute_from_outside_v2(
        ref self: TState, outside_execution: OutsideExecution, signature: Span<felt252>,
    ) -> Array<Span<felt252>>;
    fn is_valid_outside_execution_nonce(self: @TState, nonce: felt252) -> bool;

    // IDeclarer
    fn __validate_declare__(self: @TState, class_hash: felt252) -> felt252;

    // IEthDeployable
    fn __validate_deploy__(
        self: @TState,
        class_hash: felt252,
        contract_address_salt: felt252,
        public_key: EthPublicKey,
    ) -> felt252;

    // IEthPublicKey
    fn get_public_key(self: @TState) -> EthPublicKey;
    fn set_public_key(ref self: TState, new_public_key: EthPublicKey, signature: Span<felt252>);

    // ISRC6CamelOnly
    fn isValidSignature(self: @TState, hash: felt252, signature: Array<felt252>) -> felt252;

    // IEthPublicKeyCamel
    fn getPublicKey(self: @TState) -> EthPublicKey;
    fn setPublicKey(ref self: TState, newPublicKey: EthPublicKey, signature: Span<felt252>);

    // IUpgradeable
    fn upgrade(ref self: TState, new_class_hash: ClassHash);
}
