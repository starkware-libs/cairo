use starknet::ClassHash;


pub const EXTERNAL_COMPONENT_VAULT: felt252 = 'VAULTS';
pub const EXTERNAL_COMPONENT_WITHDRAWALS: felt252 = 'WITHDRAWALS';
pub const EXTERNAL_COMPONENT_TRANSFERS: felt252 = 'TRANSFERS';
pub const EXTERNAL_COMPONENT_LIQUIDATIONS: felt252 = 'LIQUIDATIONS';
pub const EXTERNAL_COMPONENT_DELEVERAGES: felt252 = 'DELEVERAGES';
pub const EXTERNAL_COMPONENT_DEPOSITS: felt252 = 'DEPOSITS';
pub const EXTERNAL_COMPONENT_ASSETS: felt252 = 'ASSETS';

#[starknet::interface]
pub trait IExternalComponents<TContractState> {
    fn register_external_component(
        ref self: TContractState, component_type: felt252, component_address: ClassHash,
    );

    fn activate_external_component(
        ref self: TContractState, component_type: felt252, component_address: ClassHash,
    );
}

