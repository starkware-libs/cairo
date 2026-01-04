use perpetuals::core::types::asset::AssetId;
use perpetuals::core::types::position::PositionId;
use starkware_utils::signature::stark::Signature;
use starkware_utils::time::time::Timestamp;

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct TransferRequest {
    #[key]
    pub recipient: PositionId,
    #[key]
    pub position_id: PositionId,
    pub collateral_id: AssetId,
    pub amount: u64,
    pub expiration: Timestamp,
    #[key]
    pub transfer_request_hash: felt252,
    pub salt: felt252,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct Transfer {
    #[key]
    pub recipient: PositionId,
    #[key]
    pub position_id: PositionId,
    pub collateral_id: AssetId,
    pub amount: u64,
    pub expiration: Timestamp,
    #[key]
    pub transfer_request_hash: felt252,
    pub salt: felt252,
}

#[starknet::interface]
pub trait ITransferManager<TContractState> {
    fn transfer_request(
        ref self: TContractState,
        signature: Signature,
        asset_id: AssetId,
        recipient: PositionId,
        position_id: PositionId,
        amount: u64,
        expiration: Timestamp,
        salt: felt252,
    );
    fn transfer(
        ref self: TContractState,
        operator_nonce: u64,
        asset_id: AssetId,
        recipient: PositionId,
        position_id: PositionId,
        amount: u64,
        expiration: Timestamp,
        salt: felt252,
    );
}

#[starknet::contract]
pub(crate) mod TransferManager {
    use core::num::traits::Zero;
    use openzeppelin::access::accesscontrol::AccessControlComponent;
    use openzeppelin::introspection::src5::SRC5Component;
    use perpetuals::core::components::assets::AssetsComponent;
    use perpetuals::core::components::assets::AssetsComponent::InternalImpl as AssetsInternal;
    use perpetuals::core::components::assets::interface::IAssets;
    use perpetuals::core::components::fulfillment::fulfillment::Fulfillement as FulfillmentComponent;
    use perpetuals::core::components::operator_nonce::OperatorNonceComponent;
    use perpetuals::core::components::operator_nonce::OperatorNonceComponent::InternalImpl as OperatorNonceInternal;
    use perpetuals::core::components::positions::Positions as PositionsComponent;
    use perpetuals::core::components::positions::Positions::InternalTrait as PositionsInternal;
    use perpetuals::core::types::asset::AssetId;
    use perpetuals::core::types::position::{PositionId, PositionTrait};
    use starknet::storage::StoragePointerReadAccess;
    use starkware_utils::components::pausable::PausableComponent;
    use starkware_utils::components::pausable::PausableComponent::InternalImpl as PausableInternal;
    use starkware_utils::components::request_approvals::RequestApprovalsComponent;
    use starkware_utils::components::request_approvals::RequestApprovalsComponent::InternalTrait as RequestApprovalsInternal;
    use starkware_utils::components::roles::RolesComponent;
    use starkware_utils::storage::iterable_map::{
        IterableMapIntoIterImpl, IterableMapReadAccessImpl, IterableMapWriteAccessImpl,
    };
    use starkware_utils::time::time::validate_expiration;
    use crate::core::components::external_components::interface::EXTERNAL_COMPONENT_TRANSFERS;
    use crate::core::components::external_components::named_component::ITypedComponent;
    use crate::core::components::snip::SNIP12MetadataImpl;
    use crate::core::components::vaults::vaults::{IVaults, Vaults as VaultsComponent};
    use crate::core::errors::{INVALID_SAME_POSITIONS, INVALID_ZERO_AMOUNT, SIGNED_TX_EXPIRED};
    use crate::core::types::asset::synthetic::AssetType;
    use crate::core::types::position::PositionDiff;
    use crate::core::types::transfer::TransferArgs;
    use super::{ITransferManager, Signature, Timestamp, Transfer, TransferRequest};

    impl SnipImpl = SNIP12MetadataImpl;


    #[event]
    #[derive(Drop, starknet::Event)]
    pub enum Event {
        Transfer: Transfer,
        TransferRequest: TransferRequest,
        #[flat]
        FulfillmentEvent: FulfillmentComponent::Event,
        #[flat]
        PausableEvent: PausableComponent::Event,
        #[flat]
        OperatorNonceEvent: OperatorNonceComponent::Event,
        #[flat]
        AssetsEvent: AssetsComponent::Event,
        #[flat]
        PositionsEvent: PositionsComponent::Event,
        #[flat]
        RequestApprovalsEvent: RequestApprovalsComponent::Event,
        #[flat]
        SRC5Event: SRC5Component::Event,
        #[flat]
        AccessControlEvent: AccessControlComponent::Event,
        #[flat]
        RolesEvent: RolesComponent::Event,
        #[flat]
        VaultsEvent: VaultsComponent::Event,
    }

    #[storage]
    pub struct Storage {
        #[substorage(v0)]
        accesscontrol: AccessControlComponent::Storage,
        #[substorage(v0)]
        operator_nonce: OperatorNonceComponent::Storage,
        #[substorage(v0)]
        pausable: PausableComponent::Storage,
        #[substorage(v0)]
        pub roles: RolesComponent::Storage,
        #[substorage(v0)]
        #[allow(starknet::colliding_storage_paths)]
        pub assets: AssetsComponent::Storage,
        #[substorage(v0)]
        pub positions: PositionsComponent::Storage,
        #[substorage(v0)]
        pub fulfillment_tracking: FulfillmentComponent::Storage,
        #[substorage(v0)]
        src5: SRC5Component::Storage,
        #[substorage(v0)]
        pub request_approvals: RequestApprovalsComponent::Storage,
        #[substorage(v0)]
        pub vaults: VaultsComponent::Storage,
    }

    component!(path: FulfillmentComponent, storage: fulfillment_tracking, event: FulfillmentEvent);
    component!(path: PausableComponent, storage: pausable, event: PausableEvent);
    component!(path: OperatorNonceComponent, storage: operator_nonce, event: OperatorNonceEvent);
    component!(path: AssetsComponent, storage: assets, event: AssetsEvent);
    component!(path: PositionsComponent, storage: positions, event: PositionsEvent);
    component!(path: RolesComponent, storage: roles, event: RolesEvent);
    component!(path: SRC5Component, storage: src5, event: SRC5Event);
    component!(path: AccessControlComponent, storage: accesscontrol, event: AccessControlEvent);
    component!(
        path: RequestApprovalsComponent, storage: request_approvals, event: RequestApprovalsEvent,
    );
    component!(path: VaultsComponent, storage: vaults, event: VaultsEvent);

    #[abi(embed_v0)]
    impl TypedComponent of ITypedComponent<ContractState> {
        fn component_type(ref self: ContractState) -> felt252 {
            EXTERNAL_COMPONENT_TRANSFERS
        }
    }


    #[abi(embed_v0)]
    impl TransferManagerImpl of ITransferManager<ContractState> {
        /// Executes a transfer request.
        ///
        /// Validations:
        /// - Validates the position exists.
        /// - Validates the request does not exist.
        /// - If the position has an owner account, validate that the caller is the position owner
        /// account.
        /// - Validates the signature.
        ///
        /// Execution:
        /// - Registers the transfer request.
        /// - Emits a `TransferRequest` event.
        fn transfer_request(
            ref self: ContractState,
            signature: Signature,
            asset_id: AssetId,
            recipient: PositionId,
            position_id: PositionId,
            amount: u64,
            expiration: Timestamp,
            salt: felt252,
        ) {
            if (asset_id != self.assets.get_collateral_id()) {
                let asset_config = self.assets.get_asset_config(asset_id);
                assert(
                    asset_config.asset_type == AssetType::SPOT_COLLATERAL
                        || asset_config.asset_type == AssetType::VAULT_SHARE_COLLATERAL,
                    'NOT_TRANSFERABLE_ASSET',
                );

                if (asset_config.asset_type == AssetType::VAULT_SHARE_COLLATERAL) {
                    assert(
                        !self.vaults.is_vault_position(recipient), 'TRANSFER_VAULT_SHARES_TO_VAULT',
                    );
                }
            }

            self.positions.get_position_snapshot(position_id: recipient);
            let position = self.positions.get_position_snapshot(:position_id);
            assert(amount.is_non_zero(), INVALID_ZERO_AMOUNT);
            let owner_account = if (position.owner_protection_enabled.read()) {
                position.get_owner_account()
            } else {
                Option::None
            };
            let hash = self
                .request_approvals
                .register_approval(
                    owner_account: owner_account,
                    public_key: position.get_owner_public_key(),
                    :signature,
                    args: TransferArgs {
                        position_id, recipient, salt, expiration, collateral_id: asset_id, amount,
                    },
                );
            self
                .emit(
                    TransferRequest {
                        position_id,
                        recipient,
                        collateral_id: asset_id,
                        amount,
                        expiration,
                        transfer_request_hash: hash,
                        salt,
                    },
                );
        }

        /// Executes a transfer.
        ///
        /// Validations:
        /// - The contract must not be paused.
        /// - The `operator_nonce` must be valid.
        /// - The funding validation interval has not passed since the last funding tick.
        /// - The prices of all assets in the system are valid.
        /// - Validates both the sender and recipient positions exist.
        /// - Ensures the amount is positive.
        /// - Validates the expiration time.
        /// - Validates request approval.
        ///
        /// Execution:
        /// - Adjust collateral balances.
        /// - Validates the sender position is healthy or healthier after the execution.
        fn transfer(
            ref self: ContractState,
            operator_nonce: u64,
            asset_id: AssetId,
            recipient: PositionId,
            position_id: PositionId,
            amount: u64,
            expiration: Timestamp,
            salt: felt252,
        ) {
            validate_expiration(:expiration, err: SIGNED_TX_EXPIRED);
            assert(recipient != position_id, INVALID_SAME_POSITIONS);
            let position = self.positions.get_position_snapshot(:position_id);
            let hash = self
                .request_approvals
                .consume_approved_request(
                    args: TransferArgs {
                        recipient, position_id, collateral_id: asset_id, amount, expiration, salt,
                    },
                    public_key: position.get_owner_public_key(),
                );
            self._execute_transfer(:recipient, :position_id, collateral_id: asset_id, :amount);
            self
                .emit(
                    Transfer {
                        recipient,
                        position_id,
                        collateral_id: asset_id,
                        amount,
                        expiration,
                        transfer_request_hash: hash,
                        salt,
                    },
                );
        }
    }

    #[generate_trait]
    pub impl InternalFunctions of TransferManagerFunctionsTrait {
        fn _execute_transfer(
            ref self: ContractState,
            recipient: PositionId,
            position_id: PositionId,
            collateral_id: AssetId,
            amount: u64,
        ) {
            // Parameters

            let (position_diff_sender, position_diff_recipient) = if (collateral_id == self
                .assets
                .get_collateral_id()) {
                let position_diff_sender = PositionDiff {
                    collateral_diff: -amount.into(), asset_diff: Option::None,
                };

                let position_diff_recipient = PositionDiff {
                    collateral_diff: amount.into(), asset_diff: Option::None,
                };
                (position_diff_sender, position_diff_recipient)
            } else {
                let position_diff_sender = PositionDiff {
                    collateral_diff: 0_i64.into(),
                    asset_diff: Option::Some((collateral_id, -amount.into())),
                };

                let position_diff_recipient = PositionDiff {
                    collateral_diff: 0_i64.into(),
                    asset_diff: Option::Some((collateral_id, amount.into())),
                };
                (position_diff_sender, position_diff_recipient)
            };

            /// Validations - Fundamentals:
            let sender_position = self.positions.get_position_snapshot(:position_id);
            self
                .positions
                .validate_healthy_or_healthier_position(
                    :position_id,
                    position: sender_position,
                    position_diff: position_diff_sender,
                    tvtr_before: Default::default(),
                );

            // Execute transfer
            self.positions.apply_diff(:position_id, position_diff: position_diff_sender);
            self
                .positions
                .apply_diff(position_id: recipient, position_diff: position_diff_recipient);
        }
    }
}
