#[starknet::contract]
pub mod MockValidExternalComponent {
    use core::panic_with_felt252;
    use crate::core::components::external_components::named_component::ITypedComponent;
    use crate::core::components::transfer::transfer_manager::ITransferManager;


    #[abi(embed_v0)]
    impl TypedComponent of ITypedComponent<ContractState> {
        fn component_type(ref self: ContractState) -> felt252 {
            'TRANSFERS'
        }
    }

    #[abi(embed_v0)]
    impl Transfers of ITransferManager<ContractState> {
        fn transfer_request(
            ref self: ContractState,
            signature: Span<felt252>,
            asset_id: crate::core::types::asset::AssetId,
            recipient: crate::core::types::position::PositionId,
            position_id: crate::core::types::position::PositionId,
            amount: u64,
            expiration: starkware_utils::time::time::Timestamp,
            salt: felt252,
        ) {
            panic_with_felt252('MOCK_TRANSFER_REQUEST');
        }

        fn transfer(
            ref self: ContractState,
            operator_nonce: u64,
            asset_id: crate::core::types::asset::AssetId,
            recipient: crate::core::types::position::PositionId,
            position_id: crate::core::types::position::PositionId,
            amount: u64,
            expiration: starkware_utils::time::time::Timestamp,
            salt: felt252,
        ) {
            println!("MOCK_TRANSFER");
            panic_with_felt252('MOCK_TRANSFER');
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
