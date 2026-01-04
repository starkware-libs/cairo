#[starknet::contract]
#[with_components(ERC1155, SRC5)]
pub mod DualCaseERC1155Mock {
    use openzeppelin_token::erc1155::ERC1155HooksEmptyImpl;
    use starknet::ContractAddress;

    // ERC1155
    #[abi(embed_v0)]
    impl ERC1155Impl = ERC1155Component::ERC1155Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC1155MetadataURIImpl =
        ERC1155Component::ERC1155MetadataURIImpl<ContractState>;
    #[abi(embed_v0)]
    impl ERC721Camel = ERC1155Component::ERC1155CamelImpl<ContractState>;

    // SRC5
    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[constructor]
    fn constructor(
        ref self: ContractState,
        base_uri: ByteArray,
        recipient: ContractAddress,
        token_id: u256,
        value: u256,
    ) {
        self.erc1155.initializer(base_uri);
        self.erc1155.mint_with_acceptance_check(recipient, token_id, value, array![].span());
    }
}

#[starknet::contract]
#[with_components(ERC1155, SRC5)]
pub mod SnakeERC1155Mock {
    use openzeppelin_token::erc1155::ERC1155HooksEmptyImpl;
    use starknet::ContractAddress;

    // ERC1155
    #[abi(embed_v0)]
    impl ERC1155Impl = ERC1155Component::ERC1155Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC1155MetadataURIImpl =
        ERC1155Component::ERC1155MetadataURIImpl<ContractState>;

    // SRC5
    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[constructor]
    fn constructor(
        ref self: ContractState,
        base_uri: ByteArray,
        recipient: ContractAddress,
        token_id: u256,
        value: u256,
    ) {
        self.erc1155.initializer(base_uri);
        self.erc1155.mint_with_acceptance_check(recipient, token_id, value, array![].span());
    }
}

/// Similar to `SnakeERC1155Mock`, but emits events for `before_update` and `after_update` hooks.
/// This is used to test that the hooks are called with the correct arguments.
#[starknet::contract]
#[with_components(ERC1155, SRC5)]
pub mod SnakeERC1155MockWithHooks {
    use starknet::ContractAddress;

    // ERC1155
    #[abi(embed_v0)]
    impl ERC1155Impl = ERC1155Component::ERC1155Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC1155MetadataURIImpl =
        ERC1155Component::ERC1155MetadataURIImpl<ContractState>;

    // SRC5
    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[event]
    #[derive(Drop, starknet::Event)]
    pub enum Event {
        BeforeUpdate: BeforeUpdate,
        AfterUpdate: AfterUpdate,
    }

    /// Event used to test that `before_update` hook is called.
    #[derive(Drop, PartialEq, starknet::Event)]
    pub struct BeforeUpdate {
        pub from: ContractAddress,
        pub to: ContractAddress,
        pub token_ids: Span<u256>,
        pub values: Span<u256>,
    }

    /// Event used to test that `after_update` hook is called.
    #[derive(Drop, PartialEq, starknet::Event)]
    pub struct AfterUpdate {
        pub from: ContractAddress,
        pub to: ContractAddress,
        pub token_ids: Span<u256>,
        pub values: Span<u256>,
    }

    #[constructor]
    fn constructor(
        ref self: ContractState,
        base_uri: ByteArray,
        recipient: ContractAddress,
        token_id: u256,
        value: u256,
    ) {
        self.erc1155.initializer(base_uri);
        self.erc1155.mint_with_acceptance_check(recipient, token_id, value, array![].span());
    }

    impl ERC1155HooksImpl of ERC1155Component::ERC1155HooksTrait<ContractState> {
        fn before_update(
            ref self: ERC1155Component::ComponentState<ContractState>,
            from: ContractAddress,
            to: ContractAddress,
            token_ids: Span<u256>,
            values: Span<u256>,
        ) {
            let mut contract_state = self.get_contract_mut();
            contract_state.emit(BeforeUpdate { from, to, token_ids, values });
        }

        fn after_update(
            ref self: ERC1155Component::ComponentState<ContractState>,
            from: ContractAddress,
            to: ContractAddress,
            token_ids: Span<u256>,
            values: Span<u256>,
        ) {
            let mut contract_state = self.get_contract_mut();
            contract_state.emit(AfterUpdate { from, to, token_ids, values });
        }
    }
}

#[starknet::contract]
#[with_components(ERC1155Receiver, SRC5)]
pub mod DualCaseERC1155ReceiverMock {
    // ERC1155Receiver Mixin
    #[abi(embed_v0)]
    impl ERC1155ReceiverMixinImpl =
        ERC1155ReceiverComponent::ERC1155ReceiverMixinImpl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[constructor]
    fn constructor(ref self: ContractState) {
        self.erc1155_receiver.initializer();
    }
}
