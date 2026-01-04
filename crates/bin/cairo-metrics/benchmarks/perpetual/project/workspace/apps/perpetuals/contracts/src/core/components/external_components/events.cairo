use starknet::ClassHash;
use starkware_utils::time::time::Timestamp;


#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct ExternalComponentImplRegistered {
    #[key]
    pub component_type: felt252,
    pub activation_time: Timestamp,
    pub implementation: ClassHash,
}


#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct ExternalComponentImplActivated {
    #[key]
    pub component_type: felt252,
    pub implementation: ClassHash,
}
