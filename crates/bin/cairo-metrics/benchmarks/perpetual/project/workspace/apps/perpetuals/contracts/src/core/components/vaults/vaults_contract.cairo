use perpetuals::core::types::asset::AssetId;
use perpetuals::core::types::order::LimitOrder;
use perpetuals::core::types::position::PositionId;
use perpetuals::core::types::vault::ConvertPositionToVault;
use starkware_utils::signature::stark::Signature;


#[starknet::interface]
pub trait IVaultExternal<TContractState> {
    fn activate_vault(
        ref self: TContractState,
        operator_nonce: u64,
        order: ConvertPositionToVault,
        signature: Signature,
    );
    fn invest_in_vault(
        ref self: TContractState,
        operator_nonce: u64,
        signature: Signature,
        order: LimitOrder,
        correlation_id: felt252,
    );
    fn redeem_from_vault(
        ref self: TContractState,
        operator_nonce: u64,
        signature: Signature,
        order: LimitOrder,
        vault_approval: LimitOrder,
        vault_signature: Signature,
        actual_shares_user: i64,
        actual_collateral_user: i64,
    );

    fn liquidate_vault_shares(
        ref self: TContractState,
        operator_nonce: u64,
        liquidated_position_id: PositionId,
        vault_approval: LimitOrder,
        vault_signature: Signature,
        liquidated_asset_id: AssetId,
        actual_shares_user: i64,
        actual_collateral_user: i64,
    );
}

#[starknet::contract]
pub(crate) mod VaultsManager {
    use AssetsComponent::InternalTrait;
    use core::num::traits::{WideMul, Zero};
    use core::panics::panic_with_byte_array;
    use openzeppelin::access::accesscontrol::AccessControlComponent;
    use openzeppelin::interfaces::erc20::{IERC20Dispatcher, IERC20DispatcherTrait};
    use openzeppelin::interfaces::erc4626::{IERC4626Dispatcher, IERC4626DispatcherTrait};
    use openzeppelin::introspection::src5::SRC5Component;
    use perpetuals::core::components::assets::AssetsComponent;
    use perpetuals::core::components::assets::interface::IAssets;
    use perpetuals::core::components::deposit::Deposit as DepositComponent;
    use perpetuals::core::components::deposit::Deposit::InternalImpl as DepositInternal;
    use perpetuals::core::components::fulfillment::fulfillment::Fulfillement as FulfillmentComponent;
    use perpetuals::core::components::fulfillment::interface::IFulfillment;
    use perpetuals::core::components::operator_nonce::OperatorNonceComponent;
    use perpetuals::core::components::positions::Positions as PositionsComponent;
    use perpetuals::core::components::positions::Positions::InternalTrait as PositionsInternal;
    use perpetuals::core::types::asset::AssetId;
    use perpetuals::core::types::position::{PositionId, PositionTrait};
    use perpetuals::core::types::price::PriceMulTrait;
    use perpetuals::core::types::risk_factor::RiskFactorMulTrait;
    use starkware_utils::components::pausable::PausableComponent;
    use starkware_utils::components::request_approvals::RequestApprovalsComponent;
    use starkware_utils::components::roles::RolesComponent;
    use starkware_utils::math::abs::Abs;
    use starkware_utils::storage::iterable_map::{
        IterableMapIntoIterImpl, IterableMapReadAccessImpl, IterableMapWriteAccessImpl,
    };
    use starkware_utils::time::time::Time;
    use vault::interface::{IProtocolVaultDispatcher, IProtocolVaultDispatcherTrait};
    use crate::core::components::deposit::deposit_manager::IDepositExternalDispatcherTrait;
    use crate::core::components::external_components::external_component_manager::ExternalComponents as ExternalComponentsComponent;
    use crate::core::components::external_components::external_component_manager::ExternalComponents::InternalImpl as ExternalComponentsInternal;
    use crate::core::components::external_components::interface::EXTERNAL_COMPONENT_VAULT;
    use crate::core::components::external_components::named_component::ITypedComponent;
    use crate::core::components::positions::interface::IPositions;
    use crate::core::components::snip::SNIP12MetadataImpl;
    use crate::core::components::vaults::events;
    use crate::core::components::vaults::vaults::Vaults::InternalTrait as VaultsInternal;
    use crate::core::components::vaults::vaults::{IVaults, Vaults as VaultsComponent};
    use crate::core::errors::order_expired_err;
    use crate::core::types::order::ValidateableOrderTrait;
    use crate::core::types::position::PositionDiff;
    use crate::core::utils::{validate_signature, validate_trade};
    use super::{ConvertPositionToVault, IVaultExternal, LimitOrder, Signature};


    #[event]
    #[derive(Drop, starknet::Event)]
    pub enum Event {
        InvestInVault: events::InvestInVault,
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
        DepositEvent: DepositComponent::Event,
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
        #[flat]
        ExternalComponentsEvent: ExternalComponentsComponent::Event,
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
        pub deposits: DepositComponent::Storage,
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
        #[substorage(v0)]
        pub external_components: ExternalComponentsComponent::Storage,
    }

    component!(path: FulfillmentComponent, storage: fulfillment_tracking, event: FulfillmentEvent);
    component!(path: PausableComponent, storage: pausable, event: PausableEvent);
    component!(path: OperatorNonceComponent, storage: operator_nonce, event: OperatorNonceEvent);
    component!(path: AssetsComponent, storage: assets, event: AssetsEvent);
    component!(path: PositionsComponent, storage: positions, event: PositionsEvent);
    component!(path: DepositComponent, storage: deposits, event: DepositEvent);
    component!(path: RolesComponent, storage: roles, event: RolesEvent);
    component!(path: SRC5Component, storage: src5, event: SRC5Event);
    component!(path: AccessControlComponent, storage: accesscontrol, event: AccessControlEvent);
    component!(
        path: RequestApprovalsComponent, storage: request_approvals, event: RequestApprovalsEvent,
    );

    component!(path: VaultsComponent, storage: vaults, event: VaultsEvent);

    component!(
        path: ExternalComponentsComponent,
        storage: external_components,
        event: ExternalComponentsEvent,
    );


    #[abi(embed_v0)]
    impl TypedComponent of ITypedComponent<ContractState> {
        fn component_type(ref self: ContractState) -> felt252 {
            EXTERNAL_COMPONENT_VAULT
        }
    }

    #[abi(embed_v0)]
    impl VaultsImpl of IVaultExternal<ContractState> {
        fn activate_vault(
            ref self: ContractState,
            operator_nonce: u64,
            order: ConvertPositionToVault,
            signature: Signature,
        ) {
            self
                .vaults
                .activate_vault(operator_nonce: operator_nonce, order: order, signature: signature);
        }

        fn invest_in_vault(
            ref self: ContractState,
            operator_nonce: u64,
            signature: Signature,
            order: LimitOrder,
            correlation_id: felt252,
        ) {
            let vault_config = self.vaults.get_vault_config_for_asset(order.base_asset_id);
            let from_position_id = order.source_position;
            let vault_position_id = vault_config.position_id.into();

            /// Validations - Order Parameters:
            assert(order.base_amount > 0, 'INVALID_NEGATIVE_BASE_AMOUNT');
            assert(order.quote_amount < 0, 'INVALID_POSITIVE_QUOTE_AMOUNT');
            // Expiration check.
            let now = Time::now();
            if (now > order.expiration) {
                let err = order_expired_err(from_position_id);
                panic_with_byte_array(err: @err);
            }
            assert(order.quote_asset_id == self.assets.get_collateral_id(), 'INVALID_QUOTE_ASSET');

            let receiving_position_id = order.receive_position;
            let salt = order.salt;

            let sending_position_snapshot = self
                .positions
                .get_position_snapshot(position_id: from_position_id);

            let order_hash = validate_signature(
                public_key: sending_position_snapshot.get_owner_public_key(),
                message: order,
                signature: signature,
            );

            self
                .fulfillment_tracking
                .update_fulfillment(
                    position_id: from_position_id,
                    hash: order_hash,
                    order_base_amount: order.quote_amount,
                    actual_base_amount: order.quote_amount,
                );

            assert(!self.vaults.is_vault_position(from_position_id), 'FROM_POSITION_IS_VAULT');
            assert(
                !self.vaults.is_vault_position(receiving_position_id),
                'RECEIVING_POSITION_IS_VAULT',
            );
            let vault_share_config = self.assets.get_asset_config(vault_config.asset_id);

            let vault_dispatcher = IERC4626Dispatcher {
                contract_address: vault_share_config.token_contract.expect('NOT_ERC4626'),
            };
            let collateral_token_dispatcher = self.assets.get_base_collateral_token_contract();
            let current_collateral_balance = collateral_token_dispatcher
                .balance_of(starknet::get_contract_address());

            let on_chain_amount: u128 = order
                .quote_amount
                .abs()
                .wide_mul(self.assets.get_collateral_quantum());

            collateral_token_dispatcher
                .approve(
                    spender: vault_dispatcher.contract_address, amount: on_chain_amount.into(),
                );

            let minted_shares: u128 = vault_dispatcher
                .deposit(on_chain_amount.into(), starknet::get_contract_address())
                .try_into()
                .unwrap();

            let quantised_minted_shares: u64 = (minted_shares / vault_share_config.quantum.into())
                .try_into()
                .unwrap();

            order
                .validate_against_actual_amounts(
                    actual_amount_base: quantised_minted_shares.try_into().unwrap(),
                    actual_amount_quote: order.quote_amount,
                    actual_fee: 0_u64,
                );

            let sending_position_diff = PositionDiff {
                collateral_diff: order.quote_amount.into(), asset_diff: Option::None,
            };

            self
                .positions
                .validate_healthy_or_healthier_position(
                    position_id: from_position_id,
                    position: sending_position_snapshot,
                    position_diff: sending_position_diff,
                    tvtr_before: Default::default(),
                );

            self
                .positions
                .apply_diff(position_id: from_position_id, position_diff: sending_position_diff);

            // 1. check that the collateral balance of the perps contract is returned to the
            // same amount as before deposit into vault
            // 2. apply diff to the vault position
            let new_collateral_balance = collateral_token_dispatcher
                .balance_of(starknet::get_contract_address());
            assert(new_collateral_balance == current_collateral_balance, 'COLLATERAL_NOT_RETURNED');

            let vault_position_diff = PositionDiff {
                collateral_diff: order.quote_amount.abs().into(), asset_diff: Option::None,
            };

            self
                .positions
                .apply_diff(position_id: vault_position_id, position_diff: vault_position_diff);

            let vault_token_erc20_dispatcher = IERC20Dispatcher {
                contract_address: vault_share_config.token_contract.expect('NOT_ERC20'),
            };

            vault_token_erc20_dispatcher
                .approve(
                    spender: starknet::get_contract_address(),
                    amount: quantised_minted_shares.into(),
                );

            self
                .emit(
                    events::InvestInVault {
                        vault_position_id: vault_position_id,
                        investing_position_id: from_position_id,
                        receiving_position_id: receiving_position_id,
                        shares_received: quantised_minted_shares,
                        user_investment: order.quote_amount.abs(),
                        vault_asset_id: vault_config.asset_id,
                        invested_asset_id: self.assets.get_collateral_id(),
                        correlation_id: correlation_id,
                    },
                );

            self
                .external_components
                ._get_deposit_manager_dispatcher()
                .deposit(
                    caller_address: starknet::get_contract_address(),
                    asset_id: vault_config.asset_id,
                    position_id: receiving_position_id,
                    quantized_amount: quantised_minted_shares,
                    now: now,
                    salt: salt,
                );
        }
        fn redeem_from_vault(
            ref self: ContractState,
            operator_nonce: u64,
            signature: Span<felt252>,
            order: LimitOrder,
            vault_approval: LimitOrder,
            vault_signature: Span<felt252>,
            actual_shares_user: i64,
            actual_collateral_user: i64,
        ) {
            self
                ._execute_redeem(
                    order: order,
                    :vault_approval,
                    :vault_signature,
                    :actual_shares_user,
                    :actual_collateral_user,
                    validate_user_order: true,
                    user_signature: signature,
                );
        }

        fn liquidate_vault_shares(
            ref self: ContractState,
            operator_nonce: u64,
            liquidated_position_id: PositionId,
            vault_approval: LimitOrder,
            vault_signature: Span<felt252>,
            liquidated_asset_id: AssetId,
            actual_shares_user: i64,
            actual_collateral_user: i64,
        ) {
            assert(
                self.positions.is_liquidatable(liquidated_position_id), 'POSITION_NOT_LIQUIDATABLE',
            );

            let user_order = LimitOrder {
                source_position: liquidated_position_id,
                receive_position: liquidated_position_id,
                base_asset_id: liquidated_asset_id,
                base_amount: actual_shares_user,
                quote_asset_id: self.assets.get_collateral_id(),
                quote_amount: actual_collateral_user,
                fee_asset_id: self.assets.get_collateral_id(),
                fee_amount: 0_u64,
                salt: Zero::zero(),
                expiration: Time::now(),
            };

            self
                ._execute_redeem(
                    order: user_order,
                    :vault_approval,
                    :vault_signature,
                    :actual_shares_user,
                    :actual_collateral_user,
                    validate_user_order: false,
                    user_signature: array![0, 0].span(),
                );
        }
    }

    #[generate_trait]
    pub impl InternalFunctions of VaultsFunctionsTrait {
        fn _execute_redeem(
            ref self: ContractState,
            order: LimitOrder,
            vault_approval: LimitOrder,
            vault_signature: Signature,
            actual_shares_user: i64,
            actual_collateral_user: i64,
            validate_user_order: bool,
            user_signature: Signature,
        ) {
            let vault_config = self.vaults.get_vault_config_for_asset(order.base_asset_id);
            let vault_asset = self.assets.get_asset_config(vault_config.asset_id);

            let vault_position_id: PositionId = vault_config.position_id.into();
            let redeeming_position_id = order.source_position;
            let receiving_position_id = order.receive_position;

            if (actual_shares_user >= 0) {
                let err = format!("INVALID_ACTUAL_SHARES_AMOUNT: {}", actual_shares_user);
                panic_with_byte_array(err: @err);
            }

            if (actual_collateral_user < 0) {
                let err = format!("INVALID_ACTUAL_COLLATERAL_AMOUNT: {}", actual_collateral_user);
                panic_with_byte_array(err: @err);
            }

            validate_trade(
                order_a: order,
                order_b: vault_approval,
                actual_amount_base_a: actual_shares_user,
                actual_amount_quote_a: actual_collateral_user,
                actual_fee_a: 0_u64,
                actual_fee_b: 0_u64,
                asset: Some(vault_asset),
                collateral_id: self.assets.get_collateral_id(),
            );

            let vault_position = self.positions.get_position_snapshot(vault_position_id);
            let redeeming_position = self.positions.get_position_snapshot(redeeming_position_id);

            let amount_to_burn = actual_shares_user;
            let value_to_receive = actual_collateral_user;

            if (validate_user_order) {
                let order_hash = validate_signature(
                    public_key: redeeming_position.get_owner_public_key(),
                    message: order,
                    signature: user_signature,
                );
                self
                    .fulfillment_tracking
                    .update_fulfillment(
                        position_id: redeeming_position_id,
                        hash: order_hash,
                        order_base_amount: order.base_amount.try_into().unwrap(),
                        actual_base_amount: actual_shares_user.try_into().unwrap(),
                    );
            }

            let vault_order_hash = validate_signature(
                public_key: vault_position.get_owner_public_key(),
                message: vault_approval,
                signature: vault_signature,
            );

            self
                .fulfillment_tracking
                .update_fulfillment(
                    position_id: vault_position_id,
                    hash: vault_order_hash,
                    order_base_amount: vault_approval.base_amount.try_into().unwrap(),
                    actual_base_amount: -actual_shares_user.try_into().unwrap(),
                );

            let vault_dispatcher = IProtocolVaultDispatcher {
                contract_address: vault_asset.token_contract.expect('NOT_ERC20'),
            };

            let vault_erc4626_dispatcher = IERC4626Dispatcher {
                contract_address: vault_asset.token_contract.expect('NOT_ERC4626'),
            };

            let vault_erc20Dispatcher = IERC20Dispatcher {
                contract_address: vault_asset.token_contract.expect('NOT_ERC20'),
            };

            let pnl_collateral_dispatcher = self.assets.get_base_collateral_token_contract();
            let perps_contract_balance_before = pnl_collateral_dispatcher
                .balance_of(starknet::get_contract_address());

            let unquantized_amount_to_burn = amount_to_burn.abs().wide_mul(vault_asset.quantum);

            //approve the vault contract to transfer pnl collateral to itself to "send back" to
            //perps
            pnl_collateral_dispatcher
                .approve(
                    spender: vault_asset.token_contract.expect('NOT_ERC20'),
                    amount: value_to_receive.abs().into(),
                );

            //approve the vault contract transferring vault shares to itself for burning
            vault_erc20Dispatcher
                .approve(
                    spender: vault_asset.token_contract.expect('NOT_ERC20'),
                    amount: unquantized_amount_to_burn.into(),
                );

            let value_of_shares_from_er4626 = vault_erc4626_dispatcher
                .preview_redeem(unquantized_amount_to_burn.into());
            let max_value = ((value_of_shares_from_er4626 * 1100) / 1000);
            if value_to_receive.abs().into() > max_value {
                let err = format!(
                    "Redeem value too high. requested={}, actual={}, number_of_shares={}",
                    value_to_receive.abs(),
                    value_of_shares_from_er4626,
                    unquantized_amount_to_burn,
                );
                panic_with_byte_array(err: @err);
            }
            let burn_result = vault_dispatcher
                .redeem_with_price(
                    shares: unquantized_amount_to_burn.into(),
                    value_of_shares: value_to_receive.abs().into(),
                );

            if (burn_result != value_to_receive.abs().into()) {
                let err = format!(
                    "UNFAIR_REDEEM: expected {:?}, got {:?}", value_to_receive, burn_result,
                );
                panic_with_byte_array(err: @err);
            }

            let vault_position_diff = PositionDiff {
                collateral_diff: -value_to_receive.into(), asset_diff: None,
            };

            let (redeeming_position_diff, receiving_position_diff) =
                if (receiving_position_id == redeeming_position_id) {
                (
                    PositionDiff {
                        collateral_diff: value_to_receive.into(),
                        asset_diff: Some((vault_config.asset_id, amount_to_burn.into())),
                    },
                    None,
                )
            } else {
                (
                    PositionDiff {
                        asset_diff: Some((vault_config.asset_id, amount_to_burn.into())),
                        collateral_diff: 0_i64.into(),
                    },
                    Some(
                        PositionDiff { collateral_diff: value_to_receive.into(), asset_diff: None },
                    ),
                )
            };

            // vault health checks
            self
                .positions
                .validate_healthy_or_healthier_position(
                    position_id: vault_position_id,
                    position: vault_position,
                    position_diff: vault_position_diff,
                    tvtr_before: Default::default(),
                );

            self
                .positions
                .apply_diff(position_id: vault_position_id, position_diff: vault_position_diff);

            // prevent withdrawal of more collateral than is contained with the vault
            // protection against malicious operator
            self
                .positions
                .validate_asset_balance_is_not_negative(
                    position: vault_position, asset_id: self.assets.get_collateral_id(),
                );

            self
                .positions
                .validate_asset_balance_is_not_negative(
                    position: redeeming_position, asset_id: order.base_asset_id,
                );

            // user health checks
            if (self.positions.is_liquidatable(redeeming_position_id)) {
                let (asset_id, qty) = redeeming_position_diff.asset_diff.unwrap();
                let price = self.assets.get_asset_price(asset_id);
                //spot have constant risk factors
                let risk_factor = self.assets.get_asset_risk_factor(asset_id, 1_i64.into(), price);

                let value_of_shares_sold: u128 = price
                    .mul(qty)
                    .abs()
                    .try_into()
                    .expect('REDEEM_VAULT_SHARES_OVERFLOW');

                let risk_of_shares_sold: u128 = risk_factor.mul(value_of_shares_sold);
                let collateral_received: u128 = actual_collateral_user.abs().try_into().unwrap();

                if collateral_received < value_of_shares_sold - risk_of_shares_sold {
                    let err = format!(
                        "Illegal transition value_of_shares_sold={}, risk_of_shares_sold={}, collateral_received={}",
                        value_of_shares_sold,
                        risk_of_shares_sold,
                        collateral_received,
                    );
                    panic_with_byte_array(err: @err);
                }
            } else {
                self
                    .positions
                    .validate_healthy_or_healthier_position(
                        position_id: redeeming_position_id,
                        position: redeeming_position,
                        position_diff: redeeming_position_diff,
                        tvtr_before: Default::default(),
                    );
            }

            self
                .positions
                .apply_diff(
                    position_id: redeeming_position_id, position_diff: redeeming_position_diff,
                );

            // no need to validate health as can only receive collateral
            if let Option::Some(position_diff) = receiving_position_diff {
                self
                    .positions
                    .apply_diff(position_id: receiving_position_id, position_diff: position_diff);
            }

            let new_perps_contract_balance = pnl_collateral_dispatcher
                .balance_of(starknet::get_contract_address());
            assert(
                new_perps_contract_balance == perps_contract_balance_before,
                'COLLATERAL_NOT_RETURNED',
            );
        }
    }
}
