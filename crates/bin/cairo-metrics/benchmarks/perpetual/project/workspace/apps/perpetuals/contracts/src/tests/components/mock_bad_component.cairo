#[starknet::contract]
pub mod MockInvalidExternalComponent {
    use crate::core::components::external_components::named_component::ITypedComponent;


    #[abi(embed_v0)]
    impl TypedComponent of ITypedComponent<ContractState> {
        fn component_type(ref self: ContractState) -> felt252 {
            'BAD_NAME'
        }
    }


    #[storage]
    pub struct Storage {}

    #[constructor]
    pub fn constructor(ref self: ContractState) {}

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {}
}
