use perpetuals::core::types::asset::AssetId;
use perpetuals::core::types::position::PositionId;
use starkware_utils::signature::stark::Signature;
use crate::core::types::order::Order;


#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct Liquidate {
    #[key]
    pub liquidated_position_id: PositionId,
    #[key]
    pub liquidator_order_position_id: PositionId,
    pub liquidator_order_base_asset_id: AssetId,
    pub liquidator_order_base_amount: i64,
    pub liquidator_order_quote_asset_id: AssetId,
    pub liquidator_order_quote_amount: i64,
    pub liquidator_order_fee_asset_id: AssetId,
    pub liquidator_order_fee_amount: u64,
    pub actual_amount_base_liquidated: i64,
    pub actual_amount_quote_liquidated: i64,
    pub actual_liquidator_fee: u64,
    pub insurance_fund_fee_asset_id: AssetId,
    pub insurance_fund_fee_amount: u64,
    #[key]
    pub liquidator_order_hash: felt252,
}

#[starknet::interface]
pub trait ILiquidationManager<TContractState> {
    fn liquidate(
        ref self: TContractState,
        operator_nonce: u64,
        liquidator_signature: Signature,
        liquidated_position_id: PositionId,
        liquidator_order: Order,
        actual_amount_base_liquidated: i64,
        actual_amount_quote_liquidated: i64,
        actual_liquidator_fee: u64,
        liquidated_fee_amount: u64,
    );
}

#[starknet::contract]
pub(crate) mod LiquidationManager {
    use core::num::traits::Zero;
    use core::panic_with_felt252;
    use openzeppelin::access::accesscontrol::AccessControlComponent;
    use openzeppelin::introspection::src5::SRC5Component;
    use perpetuals::core::components::assets::AssetsComponent;
    use perpetuals::core::components::assets::AssetsComponent::InternalImpl as AssetsInternal;
    use perpetuals::core::components::assets::interface::IAssets;
    use perpetuals::core::components::fulfillment::fulfillment::Fulfillement as FulfillmentComponent;
    use perpetuals::core::components::fulfillment::interface::IFulfillment;
    use perpetuals::core::components::operator_nonce::OperatorNonceComponent;
    use perpetuals::core::components::operator_nonce::OperatorNonceComponent::InternalImpl as OperatorNonceInternal;
    use perpetuals::core::components::positions::Positions as PositionsComponent;
    use perpetuals::core::components::positions::Positions::{
        FEE_POSITION, INSURANCE_FUND_POSITION, InternalTrait as PositionsInternal,
    };
    use perpetuals::core::components::snip::SNIP12MetadataImpl;
    use perpetuals::core::types::position::{PositionId, PositionTrait};
    use starknet::storage::StoragePath;
    use starkware_utils::components::pausable::PausableComponent;
    use starkware_utils::components::pausable::PausableComponent::InternalImpl as PausableInternal;
    use starkware_utils::components::request_approvals::RequestApprovalsComponent;
    use starkware_utils::components::roles::RolesComponent;
    use starkware_utils::storage::iterable_map::{
        IterableMapIntoIterImpl, IterableMapReadAccessImpl, IterableMapWriteAccessImpl,
    };
    use starkware_utils::time::time::Time;
    use crate::core::components::assets::errors::SYNTHETIC_NOT_EXISTS;
    use crate::core::components::external_components::interface::EXTERNAL_COMPONENT_LIQUIDATIONS;
    use crate::core::components::external_components::named_component::ITypedComponent;
    use crate::core::errors::CANT_LIQUIDATE_IF_POSITION;
    use crate::core::types::position::{Position, PositionDiff};
    use crate::core::utils::{validate_signature, validate_trade};
    use crate::core::value_risk_calculator::liquidated_position_validations;
    use super::{ILiquidationManager, Liquidate, Order};


    #[event]
    #[derive(Drop, starknet::Event)]
    pub enum Event {
        Liquidate: Liquidate,
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
            EXTERNAL_COMPONENT_LIQUIDATIONS
        }
    }

    #[abi(embed_v0)]
    impl LiquidationManagerImpl of ILiquidationManager<ContractState> {
        /// Executes a liquidate of a user position with liquidator order.
        ///
        /// Validations:
        /// - The contract must not be paused.
        /// - The `operator_nonce` must be valid.
        /// - The funding validation interval has not passed since the last funding tick.
        /// - The prices of all assets in the system are valid.
        /// - Validates signatures for liquidator order using the public keys of it owner.
        /// - Ensures the fee amounts are positive.
        /// - Validates that the base and quote asset types match between the liquidator and
        /// liquidated orders.
        /// - Verifies the signs of amounts:
        ///   - Ensures the sign of amounts in each order is consistent.
        ///   - Ensures the signs between liquidated order and liquidator order amount are opposite.
        /// - Ensures the liquidator order fulfillment amount do not exceed its limit.
        /// - Validates that the fee ratio does not increase.
        /// - Ensures the base-to-quote amount ratio does not decrease.
        /// - Validates liquidated position is liquidatable.
        ///
        /// Execution:
        /// - Subtract the fees from each position's collateral.
        /// - Add the fees to the `fee_position`.
        /// - Update orders' position, based on `actual_amount_base`.
        /// - Adjust collateral balances.
        /// - Perform fundamental validation for both positions after the execution.
        /// - Update liquidator order fulfillment.
        fn liquidate(
            ref self: ContractState,
            operator_nonce: u64,
            liquidator_signature: Span<felt252>,
            liquidated_position_id: PositionId,
            liquidator_order: Order,
            actual_amount_base_liquidated: i64,
            actual_amount_quote_liquidated: i64,
            actual_liquidator_fee: u64,
            liquidated_fee_amount: u64,
        ) {
            assert(liquidated_position_id != INSURANCE_FUND_POSITION, CANT_LIQUIDATE_IF_POSITION);
            let liquidator_position_id = liquidator_order.position_id;
            assert(liquidator_position_id != INSURANCE_FUND_POSITION, CANT_LIQUIDATE_IF_POSITION);

            let collateral_id = self.assets.get_collateral_id();
            let liquidated_order = Order {
                position_id: liquidated_position_id,
                base_asset_id: liquidator_order.base_asset_id,
                base_amount: actual_amount_base_liquidated,
                quote_asset_id: liquidator_order.quote_asset_id,
                quote_amount: actual_amount_quote_liquidated,
                fee_asset_id: liquidator_order.fee_asset_id,
                fee_amount: liquidated_fee_amount,
                // Dummy values needed to initialize the struct and pass validation.
                salt: Zero::zero(),
                expiration: Time::now(),
            };

            let liquidated_asset = self.assets.get_asset_config(liquidated_order.base_asset_id);

            // Validations.
            validate_trade(
                order_a: liquidated_order,
                order_b: liquidator_order,
                actual_amount_base_a: actual_amount_base_liquidated,
                actual_amount_quote_a: actual_amount_quote_liquidated,
                actual_fee_a: liquidated_fee_amount,
                actual_fee_b: actual_liquidator_fee,
                asset: Some(liquidated_asset),
                collateral_id: collateral_id,
            );

            let liquidator_position = self.positions.get_position_snapshot(liquidator_position_id);
            let liquidated_position = self
                .positions
                .get_position_snapshot(position_id: liquidated_position_id);

            // Signatures validation:
            let liquidator_order_hash = validate_signature(
                public_key: liquidator_position.get_owner_public_key(),
                message: liquidator_order,
                signature: liquidator_signature,
            );

            // Validate and update fulfillment.
            self
                .fulfillment_tracking
                .update_fulfillment(
                    position_id: liquidator_position_id,
                    hash: liquidator_order_hash,
                    order_base_amount: liquidator_order.base_amount,
                    // Passing the negative of actual amounts to `liquidator_order` as it is linked
                    // to liquidated_order.
                    actual_base_amount: -actual_amount_base_liquidated,
                );

            /// Execution:
            let liquidated_position_diff = PositionDiff {
                collateral_diff: actual_amount_quote_liquidated.into()
                    - liquidated_fee_amount.into(),
                asset_diff: Option::Some(
                    (liquidator_order.base_asset_id, actual_amount_base_liquidated.into()),
                ),
            };
            // Passing the negative of actual amounts to order_b as it is linked to order_a.
            let liquidator_position_diff = PositionDiff {
                collateral_diff: -actual_amount_quote_liquidated.into()
                    - actual_liquidator_fee.into(),
                asset_diff: Option::Some(
                    (liquidator_order.base_asset_id, -actual_amount_base_liquidated.into()),
                ),
            };
            let insurance_position_diff = PositionDiff {
                collateral_diff: liquidated_fee_amount.into(), asset_diff: Option::None,
            };
            let fee_position_diff = PositionDiff {
                collateral_diff: actual_liquidator_fee.into(), asset_diff: Option::None,
            };

            /// Validations - Fundamentals:
            self
                ._validate_liquidated_position(
                    position_id: liquidated_position_id,
                    position: liquidated_position,
                    position_diff: liquidated_position_diff,
                );
            self
                .positions
                .validate_healthy_or_healthier_position(
                    position_id: liquidator_position_id,
                    position: liquidator_position,
                    position_diff: liquidator_position_diff,
                    tvtr_before: Default::default(),
                );

            // Apply Diffs.
            self
                .positions
                .apply_diff(
                    position_id: liquidated_position_id, position_diff: liquidated_position_diff,
                );

            self
                .positions
                .apply_diff(
                    position_id: liquidator_order.position_id,
                    position_diff: liquidator_position_diff,
                );

            self.positions.apply_diff(position_id: FEE_POSITION, position_diff: fee_position_diff);

            self
                .positions
                .apply_diff(
                    position_id: INSURANCE_FUND_POSITION, position_diff: insurance_position_diff,
                );

            self
                .emit(
                    Liquidate {
                        liquidated_position_id,
                        liquidator_order_position_id: liquidator_position_id,
                        liquidator_order_base_asset_id: liquidator_order.base_asset_id,
                        liquidator_order_base_amount: liquidator_order.base_amount,
                        liquidator_order_quote_asset_id: liquidator_order.quote_asset_id,
                        liquidator_order_quote_amount: liquidator_order.quote_amount,
                        liquidator_order_fee_asset_id: liquidator_order.fee_asset_id,
                        liquidator_order_fee_amount: liquidator_order.fee_amount,
                        actual_amount_base_liquidated,
                        actual_amount_quote_liquidated,
                        actual_liquidator_fee,
                        insurance_fund_fee_asset_id: collateral_id,
                        insurance_fund_fee_amount: liquidated_fee_amount,
                        liquidator_order_hash: liquidator_order_hash,
                    },
                );
        }
    }

    #[generate_trait]
    pub impl InternalFunctions of LiquidationManagerFunctionsTrait {
        fn _validate_liquidated_position(
            ref self: ContractState,
            position_id: PositionId,
            position: StoragePath<Position>,
            position_diff: PositionDiff,
        ) {
            let (synthetic_diff_id, synthetic_diff_balance) = if let Option::Some((id, balance)) =
                position_diff
                .asset_diff {
                (id, balance)
            } else {
                panic_with_felt252(SYNTHETIC_NOT_EXISTS)
            };
            self
                .positions
                ._validate_synthetic_shrinks(
                    :position, asset_id: synthetic_diff_id, amount: synthetic_diff_balance.into(),
                );
            let (provisional_delta, unchanged_assets) = self
                .positions
                .derive_funding_delta_and_unchanged_assets(:position, :position_diff);
            let synthetic_enriched_position_diff = self
                .positions
                .enrich_asset(:position, :position_diff);
            let position_diff_enriched = self
                .positions
                .enrich_collateral(
                    :position,
                    position_diff: synthetic_enriched_position_diff,
                    provisional_delta: Option::Some(provisional_delta),
                );

            liquidated_position_validations(
                :position_id, :unchanged_assets, :position_diff_enriched,
            );
        }
    }
}
