// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (token/src/erc721/erc721_receiver.cairo)

/// # ERC721Receiver Component
///
/// The ERC721Receiver component provides implementations for the IERC721Receiver
/// interface. Integrating this component allows contracts to support ERC721
/// safe transfers.
#[starknet::component]
pub mod ERC721ReceiverComponent {
    use openzeppelin_introspection::src5::SRC5Component;
    use openzeppelin_introspection::src5::SRC5Component::{
        InternalTrait as SRC5InternalTrait, SRC5Impl,
    };
    use starknet::ContractAddress;
    use crate::erc721::interface;
    use crate::erc721::interface::{IERC721Receiver, IERC721ReceiverCamel, IERC721_RECEIVER_ID};

    #[storage]
    pub struct Storage {}

    #[embeddable_as(ERC721ReceiverImpl)]
    impl ERC721Receiver<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of IERC721Receiver<ComponentState<TContractState>> {
        /// Called whenever the implementing contract receives `token_id` through
        /// a safe transfer. This function must return `IERC721_RECEIVER_ID`
        /// to confirm the token transfer.
        fn on_erc721_received(
            self: @ComponentState<TContractState>,
            operator: ContractAddress,
            from: ContractAddress,
            token_id: u256,
            data: Span<felt252>,
        ) -> felt252 {
            IERC721_RECEIVER_ID
        }
    }

    /// Adds camelCase support for `IERC721Receiver`.
    #[embeddable_as(ERC721ReceiverCamelImpl)]
    impl ERC721ReceiverCamel<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of IERC721ReceiverCamel<ComponentState<TContractState>> {
        fn onERC721Received(
            self: @ComponentState<TContractState>,
            operator: ContractAddress,
            from: ContractAddress,
            tokenId: u256,
            data: Span<felt252>,
        ) -> felt252 {
            IERC721_RECEIVER_ID
        }
    }

    #[generate_trait]
    pub impl InternalImpl<
        TContractState,
        +HasComponent<TContractState>,
        impl SRC5: SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of InternalTrait<TContractState> {
        /// Initializes the contract by registering the IERC721Receiver interface ID.
        /// This should be used inside the contract's constructor.
        fn initializer(ref self: ComponentState<TContractState>) {
            let mut src5_component = get_dep_component_mut!(ref self, SRC5);
            src5_component.register_interface(IERC721_RECEIVER_ID);
        }
    }

    #[embeddable_as(ERC721ReceiverMixinImpl)]
    impl ERC721ReceiverMixin<
        TContractState,
        +HasComponent<TContractState>,
        impl SRC5: SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of interface::ERC721ReceiverMixin<ComponentState<TContractState>> {
        // IERC721Receiver
        fn on_erc721_received(
            self: @ComponentState<TContractState>,
            operator: ContractAddress,
            from: ContractAddress,
            token_id: u256,
            data: Span<felt252>,
        ) -> felt252 {
            ERC721Receiver::on_erc721_received(self, operator, from, token_id, data)
        }

        // IERC721ReceiverCamel
        fn onERC721Received(
            self: @ComponentState<TContractState>,
            operator: ContractAddress,
            from: ContractAddress,
            tokenId: u256,
            data: Span<felt252>,
        ) -> felt252 {
            ERC721ReceiverCamel::onERC721Received(self, operator, from, tokenId, data)
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
