use core::cmp::min;
use core::dict::{Felt252Dict, Felt252DictTrait};
use core::num::traits::{Pow, Zero};
use perpetuals::core::components::positions::interface::{
    IPositionsDispatcher, IPositionsDispatcherTrait,
};
use perpetuals::core::interface::Settlement;
use perpetuals::core::types::asset::AssetId;
use perpetuals::core::types::balance::Balance;
use perpetuals::core::types::funding::FundingTick;
use perpetuals::core::types::position::{PositionData, PositionId};
use perpetuals::tests::flow_tests::perps_tests_facade::*;
use starkware_utils::constants::HOUR;
use starkware_utils::math::abs::Abs;
use crate::core::types::funding::{FUNDING_SCALE, FundingIndex};
use crate::core::types::price::PriceMulTrait;
use crate::tests::test_utils::create_token_state;


#[derive(Drop)]
pub struct FlowTestBase {
    pub facade: PerpsTestsFacade,
    position_id_gen: u32,
    key_gen: felt252,
}

#[generate_trait]
impl PrivateFlowTestBaseImpl of PrivateFlowTestBaseTrait {
    fn generate_position_id(ref self: FlowTestBase) -> PositionId {
        self.position_id_gen += 1;
        PositionId { value: self.position_id_gen }
    }

    fn generate_key(ref self: FlowTestBase) -> felt252 {
        self.key_gen += 1;
        self.key_gen
    }
}

#[generate_trait]
pub impl FlowTestBaseImpl of FlowTestBaseTrait {
    fn new() -> FlowTestBase {
        FlowTestBase {
            facade: PerpsTestsFacadeTrait::new(create_token_state()),
            position_id_gen: 100,
            key_gen: 0,
        }
    }

    fn new_user_with_position(ref self: FlowTestBase) -> User {
        let user = UserTrait::new(
            self.facade.token_state,
            secret_key: self.generate_key(),
            position_id: self.generate_position_id(),
        );
        self
            .facade
            .new_position(
                position_id: user.position_id,
                owner_public_key: user.account.key_pair.public_key,
                owner_account: user.account.address,
            );
        user
    }

    fn new_user_with_position_and_no_owner(ref self: FlowTestBase) -> User {
        let user = UserTrait::new(
            self.facade.token_state,
            secret_key: self.generate_key(),
            position_id: self.generate_position_id(),
        );
        self
            .facade
            .new_position(
                position_id: user.position_id,
                owner_public_key: user.account.key_pair.public_key,
                owner_account: user.account.address,
            );
        user
    }

    fn new_user_with_position_id(ref self: FlowTestBase, position_id: PositionId) -> User {
        let user = UserTrait::new(
            self.facade.token_state, secret_key: self.generate_key(), :position_id,
        );
        self
            .facade
            .new_position(
                position_id: user.position_id,
                owner_public_key: user.account.key_pair.public_key,
                owner_account: user.account.address,
            );
        user
    }
}

#[derive(Copy, Drop)]
pub struct OrderRequest {
    pub order_info: OrderInfo,
    pub actual_base: u64,
}

pub struct FlowTestExtended {
    pub flow_test_base: FlowTestBase,
    pub synthetics: Felt252Dict<Nullable<AssetInfo>>,
    pub fee_percentage: u8,
}

impl DestructFlowTestExtended of Destruct<FlowTestExtended> {
    fn destruct(self: FlowTestExtended) nopanic {
        let FlowTestExtended { flow_test_base: _, synthetics: _, fee_percentage: _ } = self;
    }
}

pub const BTC_ASSET: felt252 = 'BTC';
pub const ETH_ASSET: felt252 = 'ETH';
pub const STRK_ASSET: felt252 = 'STRK';
pub const SOL_ASSET: felt252 = 'SOL';
pub const DOGE_ASSET: felt252 = 'DOGE';
pub const PEPE_ASSET: felt252 = 'PEPE';
pub const ETC_ASSET: felt252 = 'ETC';
pub const TAO_ASSET: felt252 = 'TAO';
pub const XRP_ASSET: felt252 = 'XRP';
pub const ADA_ASSET: felt252 = 'ADA';

#[generate_trait]
pub impl FlowTestImpl of FlowTestExtendedTrait {
    fn new(fee_percentage: u8) -> FlowTestExtended {
        //TODO Omri: Add test with multiple risk factors for multi trade
        let risk_factor_tiers = RiskFactorTiers {
            tiers: array![100, 200, 500].span(), first_tier_boundary: 10_000, tier_size: 10_000,
        };
        let mut synthetics = Default::default();
        let mut flow_test_base = FlowTestBaseTrait::new();

        let mut initial_price = 2_u128.pow(10);
        for asset_name in array![
            BTC_ASSET, ETH_ASSET, STRK_ASSET, SOL_ASSET, DOGE_ASSET, PEPE_ASSET, ETC_ASSET,
            TAO_ASSET, XRP_ASSET, ADA_ASSET,
        ] {
            let synthetic_info = AssetInfoTrait::new(
                :asset_name, risk_factor_data: risk_factor_tiers, oracles_len: 1,
            );
            flow_test_base.facade.add_active_synthetic(@synthetic_info, :initial_price);
            synthetics.insert(asset_name, NullableTrait::new(synthetic_info));
            initial_price /= 2;
        }

        FlowTestExtended { flow_test_base, synthetics, fee_percentage }
    }

    fn new_user(ref self: FlowTestExtended) -> User {
        self.flow_test_base.new_user_with_position()
    }

    fn deposit(ref self: FlowTestExtended, user: User, amount: u64) -> DepositInfo {
        self
            .flow_test_base
            .facade
            .deposit(
                depositor: user.account, position_id: user.position_id, quantized_amount: amount,
            )
    }
    fn deposit_spot(
        ref self: FlowTestExtended, user: User, asset_id: AssetId, amount: u64,
    ) -> DepositInfo {
        self
            .flow_test_base
            .facade
            .deposit_spot(
                depositor: user.account,
                :asset_id,
                position_id: user.position_id,
                quantized_amount: amount,
            )
    }
    fn process_deposit(ref self: FlowTestExtended, deposit_info: DepositInfo) {
        self.flow_test_base.facade.process_deposit(:deposit_info)
    }
    fn cancel_deposit(ref self: FlowTestExtended, deposit_info: DepositInfo) {
        self.flow_test_base.facade.cancel_deposit(:deposit_info)
    }
    fn withdraw_request(ref self: FlowTestExtended, user: User, amount: u64) -> RequestInfo {
        self.flow_test_base.facade.withdraw_request(:user, :amount)
    }
    fn withdraw_spot_request(
        ref self: FlowTestExtended, user: User, asset_id: AssetId, amount: u64,
    ) -> RequestInfo {
        self.flow_test_base.facade.withdraw_spot_request(:user, :asset_id, :amount)
    }
    fn withdraw(ref self: FlowTestExtended, withdraw_info: RequestInfo) {
        self.flow_test_base.facade.withdraw(:withdraw_info)
    }
    fn transfer_request(
        ref self: FlowTestExtended, sender: User, recipient: User, amount: u64,
    ) -> RequestInfo {
        self.flow_test_base.facade.transfer_request(:sender, :recipient, :amount)
    }
    fn transfer(ref self: FlowTestExtended, transfer_info: RequestInfo) {
        self.flow_test_base.facade.transfer(:transfer_info)
    }
    fn deactivate_synthetic(ref self: FlowTestExtended, asset: felt252) {
        let synthetic_info = self.synthetics.get(asset);
        self.flow_test_base.facade.deactivate_synthetic(synthetic_id: synthetic_info.asset_id);
    }

    fn hourly_funding_tick(ref self: FlowTestExtended, funding_indexes: Span<(felt252, i64)>) {
        advance_time(HOUR);
        let mut funding_indexes_dict = Default::default();
        for (asset, index) in funding_indexes {
            funding_indexes_dict.insert(*asset, NullableTrait::new(*index));
        }
        let mut funding_ticks = array![];

        for asset in array![
            ADA_ASSET, BTC_ASSET, ETC_ASSET, ETH_ASSET, SOL_ASSET, TAO_ASSET, XRP_ASSET, DOGE_ASSET,
            PEPE_ASSET, STRK_ASSET,
        ] {
            let asset_id = self.synthetics.get(asset).asset_id;
            let index_ptr = funding_indexes_dict.get(asset);
            let funding_index = if index_ptr.is_null() {
                Zero::zero()
            } else {
                FundingIndex { value: index_ptr.deref() * FUNDING_SCALE }
            };

            funding_ticks.append(FundingTick { asset_id, funding_index });
        }

        self.flow_test_base.facade.funding_tick(funding_ticks: funding_ticks.span());
    }

    fn price_tick(ref self: FlowTestExtended, prices: Span<(felt252, u128)>) {
        for (asset, price) in prices {
            let synthetic_info = self.synthetics.get(*asset).deref();
            self.flow_test_base.facade.price_tick(@synthetic_info, price: *price);
        }
    }

    fn create_order_request(
        ref self: FlowTestExtended, user: User, asset: felt252, base: i64,
    ) -> OrderRequest {
        let synthetic_info = self.synthetics.get(asset).deref();
        let synthetic_price = self
            .flow_test_base
            .facade
            .get_asset_price(synthetic_id: synthetic_info.asset_id);
        let quote: i64 = PriceMulTrait::<Balance>::mul(@synthetic_price, -base.into())
            .try_into()
            .expect('Value should not overflow');
        let order_info = self
            .flow_test_base
            .facade
            .create_order(
                :user,
                base_amount: base.into(),
                base_asset_id: synthetic_info.asset_id,
                quote_amount: quote,
                fee_amount: ((quote * self.fee_percentage.into()) / 100).abs().into(),
            );
        OrderRequest { order_info, actual_base: base.abs() }
    }

    fn trade(
        ref self: FlowTestExtended, order_a: OrderRequest, order_b: OrderRequest,
    ) -> (OrderRequest, OrderRequest) {
        let (mut buy, mut sell) = if order_a.order_info.order.base_amount > 0 {
            (order_a, order_b)
        } else {
            (order_b, order_a)
        };
        let base = min(buy.actual_base, sell.actual_base)
            .try_into()
            .expect('Value should not overflow');
        let quote = base * buy.order_info.order.quote_amount / buy.order_info.order.base_amount;

        let fee_a = quote.abs()
            * buy.order_info.order.fee_amount
            / buy.order_info.order.quote_amount.abs();

        let fee_b = quote.abs()
            * sell.order_info.order.fee_amount
            / sell.order_info.order.quote_amount.abs();

        self
            .flow_test_base
            .facade
            .trade(
                order_info_a: buy.order_info,
                order_info_b: sell.order_info,
                base: base,
                quote: quote,
                :fee_a,
                :fee_b,
            );
        buy.actual_base -= base.abs();
        sell.actual_base -= base.abs();
        (buy, sell)
    }

    fn multi_trade(
        ref self: FlowTestExtended, settlements: Span<(OrderRequest, OrderRequest)>,
    ) -> Span<(OrderRequest, OrderRequest)> {
        let mut trades: Array<Settlement> = ArrayTrait::new();
        let mut orders_after_trade: Array<(OrderRequest, OrderRequest)> = ArrayTrait::new();
        for (order_a, order_b) in settlements {
            let mut flipped = false;
            let (mut buy, mut sell) = if *order_a.order_info.order.base_amount > 0 {
                (*order_a, *order_b)
            } else {
                flipped = true;
                (*order_b, *order_a)
            };
            let base = min(buy.actual_base, sell.actual_base)
                .try_into()
                .expect('Value should not overflow');
            let quote = base * buy.order_info.order.quote_amount / buy.order_info.order.base_amount;

            let fee_a = quote.abs()
                * buy.order_info.order.fee_amount
                / buy.order_info.order.quote_amount.abs();

            let fee_b = quote.abs()
                * sell.order_info.order.fee_amount
                / sell.order_info.order.quote_amount.abs();

            trades
                .append(
                    self
                        .flow_test_base
                        .facade
                        .create_settlement(
                            order_a: buy.order_info,
                            order_b: sell.order_info,
                            :base,
                            :quote,
                            :fee_a,
                            :fee_b,
                        ),
                );
            buy.actual_base -= base.abs();
            sell.actual_base -= base.abs();
            if flipped {
                orders_after_trade.append((sell, buy));
            } else {
                orders_after_trade.append((buy, sell));
            }
        }
        self.flow_test_base.facade.multi_trade(trades: trades.span());
        orders_after_trade.span()
    }

    fn liquidate(
        ref self: FlowTestExtended, liquidated_user: User, mut liquidator_order: OrderRequest,
    ) -> OrderRequest {
        let synthetic_id = liquidator_order.order_info.order.base_asset_id;
        let asset_balances: i64 = self
            .flow_test_base
            .facade
            .get_position_asset_balance(position_id: liquidated_user.position_id, :synthetic_id)
            .into();

        let base = min(asset_balances.abs(), liquidator_order.actual_base);

        let quote = base
            * liquidator_order.order_info.order.quote_amount.abs()
            / liquidator_order.order_info.order.base_amount.abs();

        let (liquidated_base, liquidated_quote) = if liquidator_order
            .order_info
            .order
            .base_amount > 0 {
            (
                -base.try_into().expect('Value should not overflow'),
                quote.try_into().expect('Value should not overflow'),
            )
        } else {
            (
                base.try_into().expect('Value should not overflow'),
                -quote.try_into().expect('Value should not overflow'),
            )
        };

        let liquidator_fee = ((quote * self.fee_percentage.into()) / 100);
        let liquidated_insurance_fee = ((quote * self.fee_percentage.into()) / 100);

        self
            .flow_test_base
            .facade
            .liquidate(
                :liquidated_user,
                liquidator_order: liquidator_order.order_info,
                :liquidated_base,
                :liquidated_quote,
                :liquidated_insurance_fee,
                :liquidator_fee,
            );

        liquidator_order.actual_base -= base;

        liquidator_order
    }

    fn deleverage(
        ref self: FlowTestExtended,
        deleveraged_user: User,
        deleverager_user: User,
        asset: felt252,
        base: i64,
        quote: i64,
    ) {
        let synthetic_info = self.synthetics.get(asset).deref();
        self
            .flow_test_base
            .facade
            .deleverage(
                :deleveraged_user,
                :deleverager_user,
                base_asset_id: synthetic_info.asset_id,
                deleveraged_base: base,
                deleveraged_quote: quote,
            );
    }

    fn reduce_asset_position(
        ref self: FlowTestExtended, asset: felt252, user_a: User, user_b: User,
    ) {
        let synthetic_info = self.synthetics.get(asset).deref();
        let balance_a: i64 = self
            .flow_test_base
            .facade
            .get_position_asset_balance(
                position_id: user_a.position_id, synthetic_id: synthetic_info.asset_id,
            )
            .into();

        let balance_b: i64 = self
            .flow_test_base
            .facade
            .get_position_asset_balance(
                position_id: user_b.position_id, synthetic_id: synthetic_info.asset_id,
            )
            .into();

        let base_amount_a = min(balance_a.abs(), balance_b.abs());
        let base_amount_a = if balance_a > 0 {
            base_amount_a.try_into().expect('Value should not overflow')
        } else {
            -base_amount_a.try_into().expect('Value should not overflow')
        };

        self
            .flow_test_base
            .facade
            .reduce_asset_position(
                position_id_a: user_a.position_id,
                position_id_b: user_b.position_id,
                base_asset_id: synthetic_info.asset_id,
                :base_amount_a,
            );
    }

    fn get_position_data(self: @FlowTestExtended, user: User) -> @PositionData {
        let dispatcher = IPositionsDispatcher {
            contract_address: *self.flow_test_base.facade.perpetuals_contract,
        };
        @dispatcher.get_position_assets(position_id: user.position_id)
    }
}

#[generate_trait]
pub impl FlowTestValidationsImpl of FlowTestExtendedValidationsTrait {
    fn validate_total_value(self: @FlowTestExtended, user: User, expected_total_value: i128) {
        self
            .flow_test_base
            .facade
            .validate_total_value(
                position_id: user.position_id, expected_total_value: expected_total_value,
            );
    }

    fn validate_total_risk(self: @FlowTestExtended, user: User, expected_total_risk: u128) {
        self
            .flow_test_base
            .facade
            .validate_total_risk(
                position_id: user.position_id, expected_total_risk: expected_total_risk,
            );
    }
}
