// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (token/src/erc1155/erc1155_receiver.cairo)

/// # ERC1155Receiver Component
///
/// The ERC1155Receiver component provides implementations for the IERC1155Receiver
/// interface. Integrating this component allows contracts to support ERC1155
/// safe transfers.
#[starknet::component]
pub mod ERC1155ReceiverComponent {
    use openzeppelin_introspection::src5::SRC5Component;
    use openzeppelin_introspection::src5::SRC5Component::{
        InternalTrait as SRC5InternalTrait, SRC5Impl,
    };
    use starknet::ContractAddress;
    use crate::erc1155::interface::{
        ERC1155ReceiverABI, IERC1155Receiver, IERC1155ReceiverCamel, IERC1155_RECEIVER_ID,
    };

    #[storage]
    pub struct Storage {}

    #[embeddable_as(ERC1155ReceiverImpl)]
    impl ERC1155Receiver<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of IERC1155Receiver<ComponentState<TContractState>> {
        /// Called whenever the implementing contract receives `value` through
        /// a safe transfer. This function must return `IERC1155_RECEIVER_ID`
        /// to confirm the token transfer.
        fn on_erc1155_received(
            self: @ComponentState<TContractState>,
            operator: ContractAddress,
            from: ContractAddress,
            token_id: u256,
            value: u256,
            data: Span<felt252>,
        ) -> felt252 {
            IERC1155_RECEIVER_ID
        }

        /// Called whenever the implementing contract receives a batch of `values` through
        /// a safe transfer. This function must return `IERC1155_RECEIVER_ID`
        /// to confirm the token transfer.
        fn on_erc1155_batch_received(
            self: @ComponentState<TContractState>,
            operator: ContractAddress,
            from: ContractAddress,
            token_ids: Span<u256>,
            values: Span<u256>,
            data: Span<felt252>,
        ) -> felt252 {
            IERC1155_RECEIVER_ID
        }
    }

    /// Adds camelCase support for `IERC1155Receiver`.
    #[embeddable_as(ERC1155ReceiverCamelImpl)]
    impl ERC1155ReceiverCamel<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of IERC1155ReceiverCamel<ComponentState<TContractState>> {
        fn onERC1155Received(
            self: @ComponentState<TContractState>,
            operator: ContractAddress,
            from: ContractAddress,
            tokenId: u256,
            value: u256,
            data: Span<felt252>,
        ) -> felt252 {
            IERC1155_RECEIVER_ID
        }

        fn onERC1155BatchReceived(
            self: @ComponentState<TContractState>,
            operator: ContractAddress,
            from: ContractAddress,
            tokenIds: Span<u256>,
            values: Span<u256>,
            data: Span<felt252>,
        ) -> felt252 {
            IERC1155_RECEIVER_ID
        }
    }

    #[generate_trait]
    pub impl InternalImpl<
        TContractState,
        +HasComponent<TContractState>,
        impl SRC5: SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of InternalTrait<TContractState> {
        /// Initializes the contract by registering the IERC1155Receiver interface ID.
        /// This should be used inside the contract's constructor.
        fn initializer(ref self: ComponentState<TContractState>) {
            let mut src5_component = get_dep_component_mut!(ref self, SRC5);
            src5_component.register_interface(IERC1155_RECEIVER_ID);
        }
    }

    #[embeddable_as(ERC1155ReceiverMixinImpl)]
    impl ERC1155ReceiverMixin<
        TContractState,
        +HasComponent<TContractState>,
        impl SRC5: SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of ERC1155ReceiverABI<ComponentState<TContractState>> {
        // IERC1155
        fn on_erc1155_received(
            self: @ComponentState<TContractState>,
            operator: ContractAddress,
            from: ContractAddress,
            token_id: u256,
            value: u256,
            data: Span<felt252>,
        ) -> felt252 {
            ERC1155Receiver::on_erc1155_received(self, operator, from, token_id, value, data)
        }

        fn on_erc1155_batch_received(
            self: @ComponentState<TContractState>,
            operator: ContractAddress,
            from: ContractAddress,
            token_ids: Span<u256>,
            values: Span<u256>,
            data: Span<felt252>,
        ) -> felt252 {
            ERC1155Receiver::on_erc1155_batch_received(
                self, operator, from, token_ids, values, data,
            )
        }

        // IERC1155Camel
        fn onERC1155Received(
            self: @ComponentState<TContractState>,
            operator: ContractAddress,
            from: ContractAddress,
            tokenId: u256,
            value: u256,
            data: Span<felt252>,
        ) -> felt252 {
            ERC1155ReceiverCamel::onERC1155Received(self, operator, from, tokenId, value, data)
        }

        fn onERC1155BatchReceived(
            self: @ComponentState<TContractState>,
            operator: ContractAddress,
            from: ContractAddress,
            tokenIds: Span<u256>,
            values: Span<u256>,
            data: Span<felt252>,
        ) -> felt252 {
            ERC1155ReceiverCamel::onERC1155BatchReceived(
                self, operator, from, tokenIds, values, data,
            )
        }

        // ISRC5
        fn supports_interface(
            self: @ComponentState<TContractState>, interface_id: felt252,
        ) -> bool {
            let src5 = get_dep_component!(self, SRC5);
            src5.supports_interface(interface_id)
        }
    }
}
