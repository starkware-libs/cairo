use perpetuals::core::types::asset::AssetId;
use perpetuals::core::types::position::PositionId;
use starknet::ContractAddress;
use starkware_utils::signature::stark::Signature;
use starkware_utils::time::time::Timestamp;

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct WithdrawRequest {
    #[key]
    pub position_id: PositionId,
    #[key]
    pub recipient: ContractAddress,
    pub collateral_id: AssetId,
    pub amount: u64,
    pub expiration: Timestamp,
    #[key]
    pub withdraw_request_hash: felt252,
    pub salt: felt252,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct Withdraw {
    #[key]
    pub position_id: PositionId,
    #[key]
    pub recipient: ContractAddress,
    pub collateral_id: AssetId,
    pub amount: u64,
    pub expiration: Timestamp,
    #[key]
    pub withdraw_request_hash: felt252,
    pub salt: felt252,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct ForcedWithdrawRequest {
    #[key]
    pub position_id: PositionId,
    #[key]
    pub recipient: ContractAddress,
    pub collateral_id: AssetId,
    pub amount: u64,
    pub expiration: Timestamp,
    #[key]
    pub forced_withdraw_request_hash: felt252,
    pub salt: felt252,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct ForcedWithdraw {
    #[key]
    pub position_id: PositionId,
    #[key]
    pub recipient: ContractAddress,
    pub collateral_id: AssetId,
    pub amount: u64,
    pub expiration: Timestamp,
    #[key]
    pub forced_withdraw_request_hash: felt252,
    pub salt: felt252,
}

#[starknet::interface]
pub trait IWithdrawalManager<TContractState> {
    fn withdraw_request(
        ref self: TContractState,
        signature: Signature,
        collateral_id: AssetId,
        recipient: ContractAddress,
        position_id: PositionId,
        amount: u64,
        expiration: Timestamp,
        salt: felt252,
    );
    fn withdraw(
        ref self: TContractState,
        collateral_id: AssetId,
        recipient: ContractAddress,
        position_id: PositionId,
        amount: u64,
        expiration: Timestamp,
        salt: felt252,
    );
    fn forced_withdraw_request(
        ref self: TContractState,
        signature: Signature,
        collateral_id: AssetId,
        recipient: ContractAddress,
        position_id: PositionId,
        amount: u64,
        expiration: Timestamp,
        salt: felt252,
    );
    fn forced_withdraw(
        ref self: TContractState,
        collateral_id: AssetId,
        recipient: ContractAddress,
        position_id: PositionId,
        amount: u64,
        expiration: Timestamp,
        salt: felt252,
    );
}

#[starknet::contract]
pub(crate) mod WithdrawalManager {
    use core::num::traits::Zero;
    use openzeppelin::access::accesscontrol::AccessControlComponent;
    use openzeppelin::interfaces::erc20::{IERC20Dispatcher, IERC20DispatcherTrait};
    use openzeppelin::introspection::src5::SRC5Component;
    use perpetuals::core::components::assets::AssetsComponent;
    use perpetuals::core::components::assets::AssetsComponent::InternalImpl as AssetsInternal;
    use perpetuals::core::components::assets::errors::{ASSET_NOT_EXISTS, INACTIVE_ASSET};
    use perpetuals::core::components::assets::interface::IAssets;
    use perpetuals::core::components::deposit::Deposit::InternalImpl as DepositInternal;
    use perpetuals::core::components::fulfillment::fulfillment::Fulfillement as FulfillmentComponent;
    use perpetuals::core::components::operator_nonce::OperatorNonceComponent;
    use perpetuals::core::components::operator_nonce::OperatorNonceComponent::InternalImpl as OperatorNonceInternal;
    use perpetuals::core::components::positions::Positions as PositionsComponent;
    use perpetuals::core::components::positions::Positions::InternalTrait as PositionsInternal;
    use perpetuals::core::components::snip::SNIP12MetadataImpl;
    use perpetuals::core::errors::{
        FORCED_WAIT_REQUIRED, INVALID_WITHDRAW_COLLATERAL, INVALID_ZERO_AMOUNT, SIGNED_TX_EXPIRED,
        TRANSFER_FAILED,
    };
    use perpetuals::core::types::asset::AssetId;
    use perpetuals::core::types::asset::synthetic::SyntheticTrait;
    use perpetuals::core::types::balance::BalanceImpl;
    use perpetuals::core::types::position::{Position, PositionDiff, PositionId, PositionTrait};
    use perpetuals::core::types::price::PriceImpl;
    use perpetuals::core::types::withdraw::{ForcedWithdrawArgs, WithdrawArgs};
    use starknet::storage::{
        StorageAsPointer, StoragePath, StoragePathEntry, StoragePointerReadAccess,
    };
    use starknet::{ContractAddress, get_block_info, get_caller_address};
    use starkware_utils::components::pausable::PausableComponent;
    use starkware_utils::components::pausable::PausableComponent::InternalImpl as PausableInternal;
    use starkware_utils::components::request_approvals::RequestApprovalsComponent;
    use starkware_utils::components::request_approvals::RequestApprovalsComponent::InternalTrait as RequestApprovalsInternal;
    use starkware_utils::components::roles::RolesComponent;
    use starkware_utils::hash::message_hash::OffchainMessageHash;
    use starkware_utils::signature::stark::HashType;
    use starkware_utils::storage::iterable_map::{
        IterableMapIntoIterImpl, IterableMapReadAccessImpl, IterableMapWriteAccessImpl,
    };
    use starkware_utils::time::time::{Time, TimeDelta, validate_expiration};
    use crate::core::components::external_components::interface::EXTERNAL_COMPONENT_WITHDRAWALS;
    use crate::core::components::external_components::named_component::ITypedComponent;
    use crate::core::types::asset::AssetStatus;
    use crate::core::types::asset::synthetic::AssetType;
    use super::{
        ForcedWithdraw, ForcedWithdrawRequest, IWithdrawalManager, Signature, Timestamp, Withdraw,
        WithdrawRequest,
    };

    impl SnipImpl = SNIP12MetadataImpl;

    #[event]
    #[derive(Drop, starknet::Event)]
    pub enum Event {
        Withdraw: Withdraw,
        WithdrawRequest: WithdrawRequest,
        ForcedWithdraw: ForcedWithdraw,
        ForcedWithdrawRequest: ForcedWithdrawRequest,
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
        // Timelock before forced actions can be executed.
        forced_action_timelock: TimeDelta,
        // Cost for executing forced actions.
        premium_cost: u64,
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

    #[abi(embed_v0)]
    impl TypedComponent of ITypedComponent<ContractState> {
        fn component_type(ref self: ContractState) -> felt252 {
            EXTERNAL_COMPONENT_WITHDRAWALS
        }
    }

    #[abi(embed_v0)]
    impl WithdrawalManagerImpl of IWithdrawalManager<ContractState> {
        fn withdraw_request(
            ref self: ContractState,
            signature: Signature,
            collateral_id: AssetId,
            recipient: ContractAddress,
            position_id: PositionId,
            amount: u64,
            expiration: Timestamp,
            salt: felt252,
        ) {
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
                    args: WithdrawArgs {
                        position_id, salt, expiration, collateral_id, amount, recipient,
                    },
                );
            self
                .emit(
                    WithdrawRequest {
                        position_id,
                        recipient,
                        collateral_id,
                        amount,
                        expiration,
                        withdraw_request_hash: hash,
                        salt,
                    },
                );
        }

        fn withdraw(
            ref self: ContractState,
            collateral_id: AssetId,
            recipient: starknet::ContractAddress,
            position_id: PositionId,
            amount: u64,
            expiration: super::Timestamp,
            salt: felt252,
        ) {
            let position = self.positions.get_position_snapshot(:position_id);

            let hash = self
                ._withdraw(
                    :recipient,
                    :position_id,
                    :amount,
                    :expiration,
                    :salt,
                    :position,
                    :collateral_id,
                );

            self
                .emit(
                    Withdraw {
                        position_id,
                        recipient,
                        collateral_id,
                        amount,
                        expiration,
                        withdraw_request_hash: hash,
                        salt,
                    },
                );
        }

        fn forced_withdraw_request(
            ref self: ContractState,
            signature: Signature,
            collateral_id: AssetId,
            recipient: ContractAddress,
            position_id: PositionId,
            amount: u64,
            expiration: Timestamp,
            salt: felt252,
        ) {
            /// Validations:
            ///
            // Validate position exists.
            let position = self.positions.get_position_snapshot(:position_id);
            assert(amount.is_non_zero(), INVALID_ZERO_AMOUNT);

            let owner_account = if (position.owner_protection_enabled.read()) {
                position.get_owner_account()
            } else {
                Option::None
            };
            let public_key = position.get_owner_public_key();

            // Validate the withdraw request was not registered nor processed yet.
            let withdraw_args_hash = self
                .request_approvals
                .store_approval(
                    :public_key,
                    args: WithdrawArgs {
                        position_id, salt, expiration, collateral_id, amount, recipient,
                    },
                );

            // Validate the forced request signature
            let hash = self
                .request_approvals
                .register_forced_approval(
                    :owner_account,
                    :public_key,
                    :signature,
                    args: ForcedWithdrawArgs { withdraw_args_hash },
                );

            /// Executions:

            // Transfer premium_cost (forced fee) from the caller to the sequencer address.
            let premium_cost = self.premium_cost.read();
            let quantum = self.assets.get_collateral_quantum();
            let token_contract = self.assets.get_base_collateral_token_contract();

            assert(
                token_contract
                    .transfer_from(
                        sender: get_caller_address(),
                        recipient: get_block_info().sequencer_address,
                        amount: (premium_cost * quantum).into(),
                    ),
                TRANSFER_FAILED,
            );

            self
                .emit(
                    ForcedWithdrawRequest {
                        position_id,
                        recipient,
                        collateral_id,
                        amount,
                        expiration,
                        forced_withdraw_request_hash: hash,
                        salt,
                    },
                );
        }

        fn forced_withdraw(
            ref self: ContractState,
            collateral_id: AssetId,
            recipient: ContractAddress,
            position_id: PositionId,
            amount: u64,
            expiration: Timestamp,
            salt: felt252,
        ) {
            let position = self.positions.get_position_snapshot(:position_id);
            let public_key = position.get_owner_public_key();

            // Calculate forced withdraw hash
            let withdraw_args = WithdrawArgs {
                position_id, salt, expiration, collateral_id, amount, recipient,
            };
            let withdraw_args_hash = withdraw_args.get_message_hash(:public_key);

            let (request_time, forced_withdraw_request_hash) = self
                .request_approvals
                .consume_forced_approved_request(
                    args: ForcedWithdrawArgs { withdraw_args_hash }, :public_key,
                );

            let now = Time::now();
            let forced_action_timelock = self.forced_action_timelock.read();
            assert(request_time.add(forced_action_timelock) <= now, FORCED_WAIT_REQUIRED);

            self
                ._withdraw(
                    :recipient,
                    :position_id,
                    :amount,
                    :expiration,
                    :salt,
                    :position,
                    :collateral_id,
                );

            self
                .emit(
                    ForcedWithdraw {
                        position_id,
                        recipient,
                        collateral_id,
                        amount,
                        expiration,
                        forced_withdraw_request_hash,
                        salt,
                    },
                );
        }
    }

    #[generate_trait]
    impl PrivateImpl of PrivateTrait {
        fn _withdraw(
            ref self: ContractState,
            recipient: ContractAddress,
            position_id: PositionId,
            amount: u64,
            expiration: Timestamp,
            salt: felt252,
            position: StoragePath<Position>,
            collateral_id: AssetId,
        ) -> HashType {
            validate_expiration(expiration: expiration, err: SIGNED_TX_EXPIRED);

            let hash = self
                .request_approvals
                .consume_approved_request(
                    args: WithdrawArgs {
                        position_id, salt, expiration, collateral_id, amount, recipient,
                    },
                    public_key: position.get_owner_public_key(),
                );

            /// Validations - Fundamentals:
            let (position_diff, quantum, token_contract) = if collateral_id != self
                .assets
                .get_collateral_id() {
                let entry = (@self).assets.asset_config.entry(collateral_id).as_ptr();
                assert(SyntheticTrait::is_some_config(entry), ASSET_NOT_EXISTS);
                assert(
                    SyntheticTrait::at_asset_type(entry) == AssetType::SPOT_COLLATERAL,
                    INVALID_WITHDRAW_COLLATERAL,
                );
                assert(
                    SyntheticTrait::at_asset_status(entry) == AssetStatus::ACTIVE, INACTIVE_ASSET,
                );
                (
                    PositionDiff {
                        collateral_diff: Zero::zero(),
                        asset_diff: Some((collateral_id, -amount.into())),
                    },
                    SyntheticTrait::at_quantum(entry),
                    IERC20Dispatcher { contract_address: SyntheticTrait::at_token_contract(entry) },
                )
            } else {
                (
                    PositionDiff { collateral_diff: -amount.into(), asset_diff: Option::None },
                    self.assets.get_collateral_quantum(),
                    self.assets.get_base_collateral_token_contract(),
                )
            };

            self
                .positions
                .validate_healthy_or_healthier_position(
                    :position_id, :position, :position_diff, tvtr_before: Default::default(),
                );

            self.positions.apply_diff(:position_id, :position_diff);
            let withdraw_unquantized_amount = quantum * amount;
            assert(
                token_contract.transfer(:recipient, amount: withdraw_unquantized_amount.into()),
                TRANSFER_FAILED,
            );

            hash
        }
    }
}
