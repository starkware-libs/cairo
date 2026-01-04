const SUCCESS: felt252 = 'SUCCESS';

#[starknet::contract]
#[with_components(ERC721, SRC5)]
pub mod DualCaseERC721Mock {
    use openzeppelin_token::erc721::ERC721HooksEmptyImpl;
    use starknet::ContractAddress;

    // ERC721
    #[abi(embed_v0)]
    impl ERC721Impl = ERC721Component::ERC721Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC721MetadataImpl = ERC721Component::ERC721MetadataImpl<ContractState>;
    #[abi(embed_v0)]
    impl ERC721CamelOnly = ERC721Component::ERC721CamelOnlyImpl<ContractState>;
    #[abi(embed_v0)]
    impl ERC721MetadataCamelOnly =
        ERC721Component::ERC721MetadataCamelOnlyImpl<ContractState>;

    // SRC5
    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[constructor]
    fn constructor(
        ref self: ContractState,
        name: ByteArray,
        symbol: ByteArray,
        base_uri: ByteArray,
        recipient: ContractAddress,
        token_id: u256,
    ) {
        self.erc721.initializer(name, symbol, base_uri);
        self.erc721.mint(recipient, token_id);
    }
}

#[starknet::contract]
#[with_components(ERC721, SRC5)]
pub mod SnakeERC721Mock {
    use openzeppelin_token::erc721::ERC721HooksEmptyImpl;
    use starknet::ContractAddress;

    // ERC721
    #[abi(embed_v0)]
    impl ERC721Impl = ERC721Component::ERC721Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC721MetadataImpl = ERC721Component::ERC721MetadataImpl<ContractState>;

    // SRC5
    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[constructor]
    fn constructor(
        ref self: ContractState,
        name: ByteArray,
        symbol: ByteArray,
        base_uri: ByteArray,
        recipient: ContractAddress,
        token_id: u256,
    ) {
        self.erc721.initializer(name, symbol, base_uri);
        self.erc721.mint(recipient, token_id);
    }
}

/// Similar as `SnakeERC721Mock`, but emits events for `before_update` and `after_update` hooks.
/// This is used to test that the hooks are called with the correct arguments.
#[starknet::contract]
#[with_components(ERC721, SRC5)]
pub mod SnakeERC721MockWithHooks {
    use starknet::ContractAddress;

    // ERC721
    #[abi(embed_v0)]
    impl ERC721Impl = ERC721Component::ERC721Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC721MetadataImpl = ERC721Component::ERC721MetadataImpl<ContractState>;

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
        pub to: ContractAddress,
        pub token_id: u256,
        pub auth: ContractAddress,
    }

    /// Event used to test that `after_update` hook is called.
    #[derive(Drop, PartialEq, starknet::Event)]
    pub struct AfterUpdate {
        pub to: ContractAddress,
        pub token_id: u256,
        pub auth: ContractAddress,
    }

    #[constructor]
    fn constructor(
        ref self: ContractState,
        name: ByteArray,
        symbol: ByteArray,
        base_uri: ByteArray,
        recipient: ContractAddress,
        token_id: u256,
    ) {
        self.erc721.initializer(name, symbol, base_uri);
        self.erc721.mint(recipient, token_id);
    }

    impl ERC721HooksImpl of ERC721Component::ERC721HooksTrait<ContractState> {
        fn before_update(
            ref self: ERC721Component::ComponentState<ContractState>,
            to: ContractAddress,
            token_id: u256,
            auth: ContractAddress,
        ) {
            let mut contract_state = self.get_contract_mut();
            contract_state.emit(BeforeUpdate { to, token_id, auth });
        }

        fn after_update(
            ref self: ERC721Component::ComponentState<ContractState>,
            to: ContractAddress,
            token_id: u256,
            auth: ContractAddress,
        ) {
            let mut contract_state = self.get_contract_mut();
            contract_state.emit(AfterUpdate { to, token_id, auth });
        }
    }
}

#[starknet::contract]
#[with_components(ERC721Receiver, SRC5)]
pub mod DualCaseERC721ReceiverMock {
    use starknet::ContractAddress;

    // ERC721Receiver
    impl ERC721ReceiverImpl = ERC721ReceiverComponent::ERC721ReceiverImpl<ContractState>;

    // SRC5
    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[constructor]
    fn constructor(ref self: ContractState) {
        self.erc721_receiver.initializer();
    }

    #[abi(per_item)]
    #[generate_trait]
    impl ExternalImpl of ExternalTrait {
        #[external(v0)]
        fn on_erc721_received(
            self: @ContractState,
            operator: ContractAddress,
            from: ContractAddress,
            token_id: u256,
            data: Span<felt252>,
        ) -> felt252 {
            if *data.at(0) == super::SUCCESS {
                self.erc721_receiver.on_erc721_received(operator, from, token_id, data)
            } else {
                0
            }
        }

        #[external(v0)]
        fn onERC721Received(
            self: @ContractState,
            operator: ContractAddress,
            from: ContractAddress,
            tokenId: u256,
            data: Span<felt252>,
        ) -> felt252 {
            Self::on_erc721_received(self, operator, from, tokenId, data)
        }
    }
}

#[starknet::contract]
#[with_components(ERC721, ERC721Enumerable, SRC5)]
pub mod ERC721EnumerableMock {
    use starknet::ContractAddress;

    // ERC721
    #[abi(embed_v0)]
    impl ERC721MixinImpl = ERC721Component::ERC721Impl<ContractState>;

    // ERC721Enumerable
    #[abi(embed_v0)]
    impl ERC721EnumerableImpl =
        ERC721EnumerableComponent::ERC721EnumerableImpl<ContractState>;

    // SRC5
    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    pub struct Storage {}

    impl ERC721HooksImpl of ERC721Component::ERC721HooksTrait<ContractState> {
        fn before_update(
            ref self: ERC721Component::ComponentState<ContractState>,
            to: ContractAddress,
            token_id: u256,
            auth: ContractAddress,
        ) {
            let mut contract_state = self.get_contract_mut();
            contract_state.erc721_enumerable.before_update(to, token_id);
        }
    }

    #[generate_trait]
    #[abi(per_item)]
    impl ExternalImpl of ExternalTrait {
        #[external(v0)]
        fn all_tokens_of_owner(self: @ContractState, owner: ContractAddress) -> Span<u256> {
            self.erc721_enumerable.all_tokens_of_owner(owner)
        }
    }

    #[constructor]
    fn constructor(
        ref self: ContractState,
        name: ByteArray,
        symbol: ByteArray,
        base_uri: ByteArray,
        recipient: ContractAddress,
        token_id: u256,
    ) {
        self.erc721.initializer(name, symbol, base_uri);
        self.erc721_enumerable.initializer();
        self.erc721.mint(recipient, token_id);
    }
}
