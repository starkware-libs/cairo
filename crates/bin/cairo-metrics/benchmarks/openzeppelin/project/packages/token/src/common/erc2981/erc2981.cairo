// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (token/src/common/erc2981/erc2981.cairo)

/// # ERC2981 Component
///
/// Implementation of the NFT Royalty Standard, a standardized way to retrieve royalty payment
/// information.
///
/// Royalty information can be specified globally for all token ids via `set_default_royalty`,
/// and/or individually for specific token ids via `set_token_royalty`. The latter takes precedence
/// over the first.
///
/// Royalty is specified as a fraction of sale price. The denominator is set by the contract by
/// using the Immutable Component Config pattern. See
/// https://github.com/starknet-io/SNIPs/blob/main/SNIPS/snip-107.md
///
/// IMPORTANT: ERC-2981 only specifies a way to signal royalty information and does not enforce its
/// payment. See https://eips.ethereum.org/EIPS/eip-2981#optional-royalty-payments[Rationale] in the
/// ERC. Marketplaces are expected to voluntarily pay royalties together with sales.
#[starknet::component]
pub mod ERC2981Component {
    use core::num::traits::Zero;
    use openzeppelin_access::accesscontrol::AccessControlComponent;
    use openzeppelin_access::accesscontrol::AccessControlComponent::InternalTrait as AccessControlInternalTrait;
    use openzeppelin_access::ownable::OwnableComponent;
    use openzeppelin_access::ownable::OwnableComponent::InternalTrait as OwnableInternalTrait;
    use openzeppelin_introspection::src5::SRC5Component;
    use openzeppelin_introspection::src5::SRC5Component::{
        InternalTrait as SRC5InternalTrait, SRC5Impl,
    };
    use starknet::ContractAddress;
    use starknet::storage::{
        Map, StorageMapReadAccess, StorageMapWriteAccess, StoragePointerReadAccess,
        StoragePointerWriteAccess,
    };
    use crate::common::erc2981::interface;
    use crate::common::erc2981::interface::IERC2981_ID;

    /// Role for the admin responsible for managing royalty settings.
    pub const ROYALTY_ADMIN_ROLE: felt252 = selector!("ROYALTY_ADMIN_ROLE");

    // This default denominator is only used when the DefaultConfig
    // is in scope in the implementing contract.
    pub const DEFAULT_FEE_DENOMINATOR: u128 = 10_000;

    #[derive(Serde, Drop, starknet::Store)]
    pub struct RoyaltyInfo {
        pub receiver: ContractAddress,
        pub royalty_fraction: u128,
    }

    #[storage]
    pub struct Storage {
        pub ERC2981_default_royalty_info: RoyaltyInfo,
        pub ERC2981_token_royalty_info: Map<u256, RoyaltyInfo>,
    }

    pub mod Errors {
        pub const INVALID_ROYALTY: felt252 = 'ERC2981: invalid royalty';
        pub const INVALID_ROYALTY_RECEIVER: felt252 = 'ERC2981: invalid receiver';
        pub const INVALID_FEE_DENOMINATOR: felt252 = 'Invalid fee denominator';
    }

    /// Constants expected to be defined at the contract level used to configure the component
    /// behaviour.
    ///
    /// - `FEE_DENOMINATOR`: The denominator with which to interpret the fee set in
    ///   `_set_token_royalty` and `_set_default_royalty` as a fraction of the sale price.
    ///
    /// Requirements:
    ///
    /// - `FEE_DENOMINATOR` must be greater than 0.
    pub trait ImmutableConfig {
        const FEE_DENOMINATOR: u128;

        fn validate() {
            assert(Self::FEE_DENOMINATOR > 0, Errors::INVALID_FEE_DENOMINATOR);
        }
    }

    //
    // External
    //

    #[embeddable_as(ERC2981Impl)]
    impl ERC2981<
        TContractState,
        +HasComponent<TContractState>,
        impl Immutable: ImmutableConfig,
        impl SRC5: SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of interface::IERC2981<ComponentState<TContractState>> {
        /// Returns how much royalty is owed and to whom, based on a sale price that may be
        /// denominated in any unit of exchange. The royalty amount is denominated and should be
        /// paid in that same unit of exchange.
        ///
        /// The returned tuple contains:
        ///
        /// - `t.0`: The receiver of the royalty payment.
        /// - `t.1`: The amount of royalty payment.
        fn royalty_info(
            self: @ComponentState<TContractState>, token_id: u256, sale_price: u256,
        ) -> (ContractAddress, u256) {
            let token_royalty_info = self.ERC2981_token_royalty_info.read(token_id);

            // If the token has no specific royalty info, use the default.
            let royalty_info = if token_royalty_info.receiver.is_zero() {
                self.ERC2981_default_royalty_info.read()
            } else {
                token_royalty_info
            };

            let royalty_amount = sale_price
                * royalty_info.royalty_fraction.into()
                / Immutable::FEE_DENOMINATOR.into();

            (royalty_info.receiver, royalty_amount)
        }
    }

    #[embeddable_as(ERC2981InfoImpl)]
    impl ERC2981Info<
        TContractState,
        +HasComponent<TContractState>,
        +ImmutableConfig,
        +SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of interface::IERC2981Info<ComponentState<TContractState>> {
        /// Returns the royalty information that all ids in this contract will default to.
        ///
        /// The returned tuple contains:
        ///
        /// - `t.0`: The receiver of the royalty payment.
        /// - `t.1`: The numerator of the royalty fraction.
        /// - `t.2`: The denominator of the royalty fraction.
        fn default_royalty(self: @ComponentState<TContractState>) -> (ContractAddress, u128, u128) {
            self._default_royalty()
        }

        /// Returns the royalty information specific to a token.
        /// If no specific royalty information is set for the token, the default is returned.
        ///
        /// The returned tuple contains:
        ///
        /// - `t.0`: The receiver of the royalty payment.
        /// - `t.1`: The numerator of the royalty fraction.
        /// - `t.2`: The denominator of the royalty fraction.
        fn token_royalty(
            self: @ComponentState<TContractState>, token_id: u256,
        ) -> (ContractAddress, u128, u128) {
            self._token_royalty(token_id)
        }
    }

    //
    // Ownable-based implementation of IERC2981Admin
    //

    #[embeddable_as(ERC2981AdminOwnableImpl)]
    impl ERC2981AdminOwnable<
        TContractState,
        +HasComponent<TContractState>,
        +ImmutableConfig,
        +SRC5Component::HasComponent<TContractState>,
        impl Ownable: OwnableComponent::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of interface::IERC2981Admin<ComponentState<TContractState>> {
        /// Sets the royalty information that all ids in this contract will default to.
        ///
        /// Requirements:
        ///
        /// - The caller is the contract owner.
        /// - `receiver` cannot be the zero address.
        /// - `fee_numerator` cannot be greater than the fee denominator.
        fn set_default_royalty(
            ref self: ComponentState<TContractState>,
            receiver: ContractAddress,
            fee_numerator: u128,
        ) {
            get_dep_component!(@self, Ownable).assert_only_owner();
            self._set_default_royalty(receiver, fee_numerator)
        }

        /// Sets the default royalty percentage and receiver to zero.
        ///
        /// Requirements:
        ///
        /// - The caller is the contract owner.
        fn delete_default_royalty(ref self: ComponentState<TContractState>) {
            get_dep_component!(@self, Ownable).assert_only_owner();
            self._delete_default_royalty()
        }

        /// Sets the royalty information for a specific token id that takes precedence over the
        /// global default.
        ///
        /// Requirements:
        ///
        /// - The caller is the contract owner.
        /// - `receiver` cannot be the zero address.
        /// - `fee_numerator` cannot be greater than the fee denominator.
        fn set_token_royalty(
            ref self: ComponentState<TContractState>,
            token_id: u256,
            receiver: ContractAddress,
            fee_numerator: u128,
        ) {
            get_dep_component!(@self, Ownable).assert_only_owner();
            self._set_token_royalty(token_id, receiver, fee_numerator)
        }

        /// Resets royalty information for the token id back to unset.
        ///
        /// Requirements:
        ///
        /// - The caller is the contract owner.
        fn reset_token_royalty(ref self: ComponentState<TContractState>, token_id: u256) {
            get_dep_component!(@self, Ownable).assert_only_owner();
            self._reset_token_royalty(token_id)
        }
    }

    //
    // AccessControl-based implementation of IERC2981Admin
    //

    #[embeddable_as(ERC2981AdminAccessControlImpl)]
    impl ERC2981AdminAccessControl<
        TContractState,
        +HasComponent<TContractState>,
        +ImmutableConfig,
        +SRC5Component::HasComponent<TContractState>,
        impl AccessControl: AccessControlComponent::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of interface::IERC2981Admin<ComponentState<TContractState>> {
        /// Sets the royalty information that all ids in this contract will default to.
        ///
        /// Requirements:
        ///
        /// - The caller must have `ROYALTY_ADMIN_ROLE` role.
        /// - `receiver` cannot be the zero address.
        /// - `fee_numerator` cannot be greater than the fee denominator.
        fn set_default_royalty(
            ref self: ComponentState<TContractState>,
            receiver: ContractAddress,
            fee_numerator: u128,
        ) {
            get_dep_component!(@self, AccessControl).assert_only_role(ROYALTY_ADMIN_ROLE);
            self._set_default_royalty(receiver, fee_numerator)
        }

        /// Sets the default royalty percentage and receiver to zero.
        ///
        /// Requirements:
        ///
        /// - The caller must have `ROYALTY_ADMIN_ROLE` role.
        fn delete_default_royalty(ref self: ComponentState<TContractState>) {
            get_dep_component!(@self, AccessControl).assert_only_role(ROYALTY_ADMIN_ROLE);
            self._delete_default_royalty()
        }

        /// Sets the royalty information for a specific token id that takes precedence over the
        /// global default.
        ///
        /// Requirements:
        ///
        /// - The caller must have `ROYALTY_ADMIN_ROLE` role.
        /// - `receiver` cannot be the zero address.
        /// - `fee_numerator` cannot be greater than the fee denominator.
        fn set_token_royalty(
            ref self: ComponentState<TContractState>,
            token_id: u256,
            receiver: ContractAddress,
            fee_numerator: u128,
        ) {
            get_dep_component!(@self, AccessControl).assert_only_role(ROYALTY_ADMIN_ROLE);
            self._set_token_royalty(token_id, receiver, fee_numerator)
        }

        /// Resets royalty information for the token id back to unset.
        ///
        /// Requirements:
        ///
        /// - The caller must have `ROYALTY_ADMIN_ROLE` role.
        fn reset_token_royalty(ref self: ComponentState<TContractState>, token_id: u256) {
            get_dep_component!(@self, AccessControl).assert_only_role(ROYALTY_ADMIN_ROLE);
            self._reset_token_royalty(token_id)
        }
    }

    //
    // Internal
    //

    #[generate_trait]
    pub impl InternalImpl<
        TContractState,
        +HasComponent<TContractState>,
        impl Immutable: ImmutableConfig,
        impl SRC5: SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of InternalTrait<TContractState> {
        /// Initializes the contract by setting the default royalty.
        ///
        /// Requirements:
        ///
        /// - `default_receiver` cannot be the zero address.
        /// - `default_royalty_fraction` cannot be greater than the fee denominator.
        /// - The fee denominator must be greater than 0.
        ///
        /// NOTE: The fee denominator is set by the contract using the Immutable Component Config.
        fn initializer(
            ref self: ComponentState<TContractState>,
            default_receiver: ContractAddress,
            default_royalty_fraction: u128,
        ) {
            Immutable::validate();

            let mut src5_component = get_dep_component_mut!(ref self, SRC5);
            src5_component.register_interface(IERC2981_ID);

            self._set_default_royalty(default_receiver, default_royalty_fraction)
        }

        /// Returns the royalty information that all ids in this contract will default to.
        ///
        /// The returned tuple contains:
        ///
        /// - `t.0`: The receiver of the royalty payment.
        /// - `t.1`: The numerator of the royalty fraction.
        /// - `t.2`: The denominator of the royalty fraction.
        fn _default_royalty(
            self: @ComponentState<TContractState>,
        ) -> (ContractAddress, u128, u128) {
            let royalty_info = self.ERC2981_default_royalty_info.read();
            (royalty_info.receiver, royalty_info.royalty_fraction, Immutable::FEE_DENOMINATOR)
        }

        /// Sets the royalty information that all ids in this contract will default to.
        ///
        /// Requirements:
        ///
        /// - `receiver` cannot be the zero address.
        /// - `fee_numerator` cannot be greater than the fee denominator.
        fn _set_default_royalty(
            ref self: ComponentState<TContractState>,
            receiver: ContractAddress,
            fee_numerator: u128,
        ) {
            let fee_denominator = Immutable::FEE_DENOMINATOR;
            assert(fee_numerator <= fee_denominator, Errors::INVALID_ROYALTY);
            assert(receiver.is_non_zero(), Errors::INVALID_ROYALTY_RECEIVER);
            self
                .ERC2981_default_royalty_info
                .write(RoyaltyInfo { receiver, royalty_fraction: fee_numerator })
        }

        /// Sets the default royalty percentage and receiver to zero.
        fn _delete_default_royalty(ref self: ComponentState<TContractState>) {
            self
                .ERC2981_default_royalty_info
                .write(RoyaltyInfo { receiver: Zero::zero(), royalty_fraction: 0 })
        }

        /// Returns the royalty information specific to a token.
        /// If no specific royalty information is set for the token, the default is returned.
        ///
        /// The returned tuple contains:
        ///
        /// - `t.0`: The receiver of the royalty payment.
        /// - `t.1`: The numerator of the royalty fraction.
        /// - `t.2`: The denominator of the royalty fraction.
        fn _token_royalty(
            self: @ComponentState<TContractState>, token_id: u256,
        ) -> (ContractAddress, u128, u128) {
            let token_royalty_info = self.ERC2981_token_royalty_info.read(token_id);

            // If the token has no specific royalty info, use the default.
            let royalty_info = if token_royalty_info.receiver.is_zero() {
                self.ERC2981_default_royalty_info.read()
            } else {
                token_royalty_info
            };

            (royalty_info.receiver, royalty_info.royalty_fraction, Immutable::FEE_DENOMINATOR)
        }

        /// Sets the royalty information for a specific token id that takes precedence over the
        /// global default.
        ///
        /// Requirements:
        ///
        /// - `receiver` cannot be the zero address.
        /// - `fee_numerator` cannot be greater than the fee denominator.
        fn _set_token_royalty(
            ref self: ComponentState<TContractState>,
            token_id: u256,
            receiver: ContractAddress,
            fee_numerator: u128,
        ) {
            let fee_denominator = Immutable::FEE_DENOMINATOR;
            assert(fee_numerator <= fee_denominator, Errors::INVALID_ROYALTY);
            assert(!receiver.is_zero(), Errors::INVALID_ROYALTY_RECEIVER);

            self
                .ERC2981_token_royalty_info
                .write(token_id, RoyaltyInfo { receiver, royalty_fraction: fee_numerator })
        }

        /// Resets royalty information for the token id back to unset.
        fn _reset_token_royalty(ref self: ComponentState<TContractState>, token_id: u256) {
            self
                .ERC2981_token_royalty_info
                .write(token_id, RoyaltyInfo { receiver: Zero::zero(), royalty_fraction: 0 })
        }
    }
}

/// Implementation of the default ERC2981Component ImmutableConfig.
///
/// See
/// https://github.com/starknet-io/SNIPs/blob/main/SNIPS/snip-107.md#defaultconfig-implementation
///
/// The default fee denominator is set to `DEFAULT_FEE_DENOMINATOR`.
pub impl DefaultConfig of ERC2981Component::ImmutableConfig {
    const FEE_DENOMINATOR: u128 = ERC2981Component::DEFAULT_FEE_DENOMINATOR;
}

#[cfg(test)]
mod tests {
    use openzeppelin_test_common::mocks::erc2981::ERC2981Mock;
    use super::ERC2981Component;
    use super::ERC2981Component::InternalImpl;

    type ComponentState = ERC2981Component::ComponentState<ERC2981Mock::ContractState>;

    fn COMPONENT_STATE() -> ComponentState {
        ERC2981Component::component_state_for_testing()
    }

    // Invalid fee denominator
    impl InvalidImmutableConfig of ERC2981Component::ImmutableConfig {
        const FEE_DENOMINATOR: u128 = 0;
    }

    #[test]
    #[should_panic(expected: 'Invalid fee denominator')]
    fn test_initializer_invalid_config_panics() {
        let mut state = COMPONENT_STATE();
        let receiver = 'DEFAULT_RECEIVER'.try_into().unwrap();

        state.initializer(receiver, 50);
    }
}
