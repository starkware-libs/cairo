// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (presets/src/erc1155.cairo)

/// # ERC1155Upgradeable Preset
///
/// The upgradeable ERC1155 contract offers a batch-mint mechanism that
/// can only be executed once upon contract construction.
///
/// For more complex or custom contracts, use Wizard for Cairo
/// https://wizard.openzeppelin.com/cairo
#[starknet::contract]
pub mod ERC1155Upgradeable {
    use openzeppelin_access::ownable::OwnableComponent;
    use openzeppelin_introspection::src5::SRC5Component;
    use openzeppelin_token::erc1155::{ERC1155Component, ERC1155HooksEmptyImpl};
    use openzeppelin_upgrades::UpgradeableComponent;
    use openzeppelin_upgrades::interface::IUpgradeable;
    use starknet::{ClassHash, ContractAddress};

    component!(path: OwnableComponent, storage: ownable, event: OwnableEvent);
    component!(path: ERC1155Component, storage: erc1155, event: ERC1155Event);
    component!(path: SRC5Component, storage: src5, event: SRC5Event);
    component!(path: UpgradeableComponent, storage: upgradeable, event: UpgradeableEvent);

    // Ownable Mixin
    #[abi(embed_v0)]
    impl OwnableMixinImpl = OwnableComponent::OwnableMixinImpl<ContractState>;
    impl OwnableInternalImpl = OwnableComponent::InternalImpl<ContractState>;

    // ERC1155 Mixin
    #[abi(embed_v0)]
    impl ERC1155MixinImpl = ERC1155Component::ERC1155MixinImpl<ContractState>;
    impl ERC1155InternalImpl = ERC1155Component::InternalImpl<ContractState>;

    // Upgradeable
    impl UpgradeableInternalImpl = UpgradeableComponent::InternalImpl<ContractState>;

    #[storage]
    pub struct Storage {
        #[substorage(v0)]
        pub ownable: OwnableComponent::Storage,
        #[substorage(v0)]
        pub erc1155: ERC1155Component::Storage,
        #[substorage(v0)]
        pub src5: SRC5Component::Storage,
        #[substorage(v0)]
        pub upgradeable: UpgradeableComponent::Storage,
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        #[flat]
        OwnableEvent: OwnableComponent::Event,
        #[flat]
        ERC1155Event: ERC1155Component::Event,
        #[flat]
        SRC5Event: SRC5Component::Event,
        #[flat]
        UpgradeableEvent: UpgradeableComponent::Event,
    }

    /// Assigns `owner` as the contract owner.
    /// Sets the `base_uri` for all tokens.
    /// Mints the `values` for `token_ids` tokens to `recipient`.
    ///
    /// Requirements:
    ///
    /// - `to` is either an account contract (supporting ISRC6) or
    ///    supports the `IERC1155Receiver` interface.
    /// - `token_ids` and `values` must have the same length.
    #[constructor]
    fn constructor(
        ref self: ContractState,
        base_uri: ByteArray,
        recipient: ContractAddress,
        token_ids: Span<u256>,
        values: Span<u256>,
        owner: ContractAddress,
    ) {
        self.ownable.initializer(owner);
        self.erc1155.initializer(base_uri);
        self
            .erc1155
            .batch_mint_with_acceptance_check(recipient, token_ids, values, array![].span());
    }

    #[abi(embed_v0)]
    impl UpgradeableImpl of IUpgradeable<ContractState> {
        /// Upgrades the contract class hash to `new_class_hash`.
        /// This may only be called by the contract owner.
        fn upgrade(ref self: ContractState, new_class_hash: ClassHash) {
            self.ownable.assert_only_owner();
            self.upgradeable.upgrade(new_class_hash);
        }
    }
}
