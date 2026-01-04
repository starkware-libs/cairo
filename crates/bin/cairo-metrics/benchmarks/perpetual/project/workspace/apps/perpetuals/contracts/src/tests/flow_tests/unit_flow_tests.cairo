use perpetuals::core::types::funding::{FUNDING_SCALE, FundingIndex, FundingTick};
use perpetuals::tests::constants::*;
use perpetuals::tests::flow_tests::infra::*;
use perpetuals::tests::flow_tests::perps_tests_facade::*;
use snforge_std::TokenTrait;
use starkware_utils::constants::MAX_U128;
use super::perps_tests_facade::PerpsTestsFacadeTrait;

#[test]
fn test_deleverage_after_funding_tick() {
    // Setup.
    let risk_factor_data = RiskFactorTiers {
        tiers: array![10].span(), first_tier_boundary: MAX_U128, tier_size: 1,
    };
    // Create a custom asset configuration to test interesting risk factor scenarios.
    let synthetic_info = AssetInfoTrait::new(
        asset_name: 'BTC_1', :risk_factor_data, oracles_len: 1,
    );
    let asset_id = synthetic_info.asset_id;

    let mut state: FlowTestBase = FlowTestBaseTrait::new();

    state.facade.add_active_synthetic(synthetic_info: @synthetic_info, initial_price: 100);

    // Create users.
    let deleveraged_user = state.new_user_with_position();
    let deleverager_user_1 = state.new_user_with_position();
    let deleverager_user_2 = state.new_user_with_position();

    // Deposit to users.
    let deposit_info_user_1 = state
        .facade
        .deposit(
            depositor: deleverager_user_1.account,
            position_id: deleverager_user_1.position_id,
            quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user_1);

    let deposit_info_user_2 = state
        .facade
        .deposit(
            depositor: deleverager_user_2.account,
            position_id: deleverager_user_2.position_id,
            quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user_2);

    // Create orders.
    // User willing to buy 2 synthetic assets for 168 (quote) + 20 (fee).
    let order_deleveraged_user = state
        .facade
        .create_order(
            user: deleveraged_user,
            base_amount: 2,
            base_asset_id: asset_id,
            quote_amount: -168,
            fee_amount: 20,
        );

    let order_deleverager_user_1 = state
        .facade
        .create_order(
            user: deleverager_user_1,
            base_amount: -1,
            base_asset_id: asset_id,
            quote_amount: 50,
            fee_amount: 2,
        );

    let order_deleverager_user_2 = state
        .facade
        .create_order(
            user: deleverager_user_2,
            base_amount: -1,
            base_asset_id: asset_id,
            quote_amount: 84,
            fee_amount: 2,
        );

    // Make trades.
    // User recieves 1 synthetic asset for 84 (quote) + 10 (fee).
    state
        .facade
        .trade(
            order_info_a: order_deleveraged_user,
            order_info_b: order_deleverager_user_1,
            base: 1,
            quote: -84,
            fee_a: 10,
            fee_b: 3,
        );

    // User recieves 1 synthetic asset for 84 (quote) + 10 (fee).
    state
        .facade
        .trade(
            order_info_a: order_deleveraged_user,
            order_info_b: order_deleverager_user_2,
            base: 1,
            quote: -84,
            fee_a: 10,
            fee_b: 1,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -188 + 2 * 100 = 12                 2 * 100 * 0.01 = 2           6
    state
        .facade
        .validate_total_value(position_id: deleveraged_user.position_id, expected_total_value: 12);
    state
        .facade
        .validate_total_risk(position_id: deleveraged_user.position_id, expected_total_risk: 2);

    advance_time(10000);
    let mut new_funding_index = FundingIndex { value: 7 * FUNDING_SCALE };
    state
        .facade
        .funding_tick(
            funding_ticks: array![
                FundingTick { asset_id: asset_id, funding_index: new_funding_index },
            ]
                .span(),
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -202 + 2 * 100 = -2                 2 * 100 * 0.01 = 2          - 1
    state
        .facade
        .validate_total_value(position_id: deleveraged_user.position_id, expected_total_value: -2);
    state
        .facade
        .validate_total_risk(position_id: deleveraged_user.position_id, expected_total_risk: 2);

    assert(
        state.facade.is_deleveragable(position_id: deleveraged_user.position_id),
        'user is not deleveragable',
    );

    state
        .facade
        .deleverage(
            deleveraged_user: deleveraged_user,
            deleverager_user: deleverager_user_1,
            base_asset_id: asset_id,
            deleveraged_base: -1,
            deleveraged_quote: 101,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -101 + 1 * 100 = -1                 1 * 100 * 0.01 = 1          - 1
    state
        .facade
        .validate_total_value(position_id: deleveraged_user.position_id, expected_total_value: -1);
    state
        .facade
        .validate_total_risk(position_id: deleveraged_user.position_id, expected_total_risk: 1);

    state
        .facade
        .deleverage(
            deleveraged_user: deleveraged_user,
            deleverager_user: deleverager_user_2,
            base_asset_id: asset_id,
            deleveraged_base: -1,
            deleveraged_quote: 101,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:     0 + 0 * 100 = 0                  0 * 100 * 0.01 = 0            -
    state
        .facade
        .validate_total_value(position_id: deleveraged_user.position_id, expected_total_value: 0);
    state
        .facade
        .validate_total_risk(position_id: deleveraged_user.position_id, expected_total_risk: 0);
}

#[test]
fn test_deleverage_after_price_tick() {
    // Setup.
    let risk_factor_data = RiskFactorTiers {
        tiers: array![100].span(), first_tier_boundary: MAX_U128, tier_size: 1,
    };
    // Create a custom asset configuration to test interesting risk factor scenarios.
    let synthetic_info = AssetInfoTrait::new(
        asset_name: 'BTC_1', :risk_factor_data, oracles_len: 1,
    );
    let asset_id = synthetic_info.asset_id;

    let mut state: FlowTestBase = FlowTestBaseTrait::new();

    state.facade.add_active_synthetic(synthetic_info: @synthetic_info, initial_price: 20);

    // Create users.
    let deleveraged_user = state.new_user_with_position();
    let deleverager_user = state.new_user_with_position();

    // Deposit to users.
    let deposit_info_user = state
        .facade
        .deposit(
            depositor: deleverager_user.account,
            position_id: deleverager_user.position_id,
            quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user);

    // Create orders.
    // User willing to buy 2 synthetic assets for 33 (quote) + 3 (fee).
    let order_deleveraged_user = state
        .facade
        .create_order(
            user: deleveraged_user,
            base_amount: 2,
            base_asset_id: asset_id,
            quote_amount: -33,
            fee_amount: 3,
        );
    let order_deleverager_user = state
        .facade
        .create_order(
            user: deleverager_user,
            base_amount: -2,
            base_asset_id: asset_id,
            quote_amount: 30,
            fee_amount: 4,
        );

    // Make trades.
    // User recieves 2 synthetic asset for 3 (quote) + 3 (fee).
    state
        .facade
        .trade(
            order_info_a: order_deleveraged_user,
            order_info_b: order_deleverager_user,
            base: 2,
            quote: -33,
            fee_a: 3,
            fee_b: 4,
        );

    //                            TV                                  TR                    TV/TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -36 + 2 * 20 = 4                    2 * 20 * 0.1 = 4               1
    state
        .facade
        .validate_total_value(position_id: deleveraged_user.position_id, expected_total_value: 4);
    state
        .facade
        .validate_total_risk(position_id: deleveraged_user.position_id, expected_total_risk: 4);

    state.facade.price_tick(asset_info: @synthetic_info, price: 10);

    //                            TV                                  TR                    TV/TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -36 + 2 * 10 = -16                  2 * 10 * 0.1 = 2               -8
    state
        .facade
        .validate_total_value(position_id: deleveraged_user.position_id, expected_total_value: -16);
    state
        .facade
        .validate_total_risk(position_id: deleveraged_user.position_id, expected_total_risk: 2);

    assert(
        state.facade.is_deleveragable(position_id: deleveraged_user.position_id),
        'user is not deleveragable',
    );

    state
        .facade
        .deleverage(
            deleveraged_user: deleveraged_user,
            deleverager_user: deleverager_user,
            base_asset_id: asset_id,
            deleveraged_base: -1,
            deleveraged_quote: 18,
        );

    //                            TV                                  TR                    TV/TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:    -18 + 1 * 10 = -8                 1 * 10 * 0.1 = 1               -8
    state
        .facade
        .validate_total_value(position_id: deleveraged_user.position_id, expected_total_value: -8);
    state
        .facade
        .validate_total_risk(position_id: deleveraged_user.position_id, expected_total_risk: 1);

    state
        .facade
        .deleverage(
            deleveraged_user: deleveraged_user,
            deleverager_user: deleverager_user,
            base_asset_id: asset_id,
            deleveraged_base: -1,
            deleveraged_quote: 18,
        );
    //                            TV                                  TR                    TV/TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:     0 + 0 * 10 = 0                     0 * 10 * 0.1 = 0              0
    state
        .facade
        .validate_total_value(position_id: deleveraged_user.position_id, expected_total_value: 0);
    state
        .facade
        .validate_total_risk(position_id: deleveraged_user.position_id, expected_total_risk: 0);
}

#[test]
fn test_deleverage_by_recieving_asset() {
    // Setup.
    let risk_factor_data = RiskFactorTiers {
        tiers: array![10].span(), first_tier_boundary: MAX_U128, tier_size: 1,
    };
    let synthetic_info = AssetInfoTrait::new(
        asset_name: 'BTC_1', :risk_factor_data, oracles_len: 1,
    );
    let asset_id = synthetic_info.asset_id;

    let mut state: FlowTestBase = FlowTestBaseTrait::new();

    state.facade.add_active_synthetic(synthetic_info: @synthetic_info, initial_price: 100);

    // Create users.
    let deleveraged_user = state.new_user_with_position();
    let deleverager_user = state.new_user_with_position();

    // Deposit to users.
    let deposit_info_user_1 = state
        .facade
        .deposit(
            depositor: deleverager_user.account,
            position_id: deleverager_user.position_id,
            quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user_1);

    // Create orders.
    let order_deleveraged_user = state
        .facade
        .create_order(
            user: deleveraged_user,
            base_amount: -2,
            base_asset_id: asset_id,
            quote_amount: 210,
            fee_amount: 0,
        );

    let order_deleverager_user = state
        .facade
        .create_order(
            user: deleverager_user,
            base_amount: 2,
            base_asset_id: asset_id,
            quote_amount: -210,
            fee_amount: 0,
        );

    // Make trades.
    state
        .facade
        .trade(
            order_info_a: order_deleveraged_user,
            order_info_b: order_deleverager_user,
            base: -2,
            quote: 210,
            fee_a: 0,
            fee_b: 0,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:    210 - 2 * 100 = 10                 2 * 100 * 0.01 = 2           5
    state
        .facade
        .validate_total_value(position_id: deleveraged_user.position_id, expected_total_value: 10);
    state
        .facade
        .validate_total_risk(position_id: deleveraged_user.position_id, expected_total_risk: 2);

    advance_time(10000);
    let mut new_funding_index = FundingIndex { value: -6 * FUNDING_SCALE };
    state
        .facade
        .funding_tick(
            funding_ticks: array![
                FundingTick { asset_id: asset_id, funding_index: new_funding_index },
            ]
                .span(),
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   198 - 2 * 100 = -2                 2 * 100 * 0.01 = 2           - 1
    state
        .facade
        .validate_total_value(position_id: deleveraged_user.position_id, expected_total_value: -2);
    state
        .facade
        .validate_total_risk(position_id: deleveraged_user.position_id, expected_total_risk: 2);

    assert(
        state.facade.is_deleveragable(position_id: deleveraged_user.position_id),
        'user is not deleveragable',
    );

    state
        .facade
        .deleverage(
            deleveraged_user: deleveraged_user,
            deleverager_user: deleverager_user,
            base_asset_id: asset_id,
            deleveraged_base: 1,
            deleveraged_quote: -99,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:     99 - 1 * 100 = -1                1 * 100 * 0.01 = 1           - 1
    state
        .facade
        .validate_total_value(position_id: deleveraged_user.position_id, expected_total_value: -1);
    state
        .facade
        .validate_total_risk(position_id: deleveraged_user.position_id, expected_total_risk: 1);

    assert(
        state.facade.is_deleveragable(position_id: deleveraged_user.position_id),
        'user is not deleveragable',
    );

    state
        .facade
        .deleverage(
            deleveraged_user: deleveraged_user,
            deleverager_user: deleverager_user,
            base_asset_id: asset_id,
            deleveraged_base: 1,
            deleveraged_quote: -99,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:     0 + 0 * 100 = 0                0 * 100 * 0.01 = 0              -
    state
        .facade
        .validate_total_value(position_id: deleveraged_user.position_id, expected_total_value: 0);
    state
        .facade
        .validate_total_risk(position_id: deleveraged_user.position_id, expected_total_risk: 0);
}

#[test]
fn test_liquidate_after_funding_tick() {
    // Setup.
    let risk_factor_data = RiskFactorTiers {
        tiers: array![10].span(), first_tier_boundary: MAX_U128, tier_size: 1,
    };
    // Create a custom asset configuration to test interesting risk factor scenarios.
    let synthetic_info = AssetInfoTrait::new(
        asset_name: 'BTC_1', :risk_factor_data, oracles_len: 1,
    );
    let asset_id = synthetic_info.asset_id;

    let mut state: FlowTestBase = FlowTestBaseTrait::new();

    state.facade.add_active_synthetic(synthetic_info: @synthetic_info, initial_price: 100);

    // Create users.
    let liquidated_user = state.new_user_with_position();
    let liquidator_user_1 = state.new_user_with_position();
    let liquidator_user_2 = state.new_user_with_position();

    // Deposit to users.
    let deposit_info_user_1 = state
        .facade
        .deposit(
            depositor: liquidator_user_1.account,
            position_id: liquidator_user_1.position_id,
            quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user_1);

    let deposit_info_user_2 = state
        .facade
        .deposit(
            depositor: liquidator_user_2.account,
            position_id: liquidator_user_2.position_id,
            quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user_2);

    // Create orders.
    // User willing to buy 3 synthetic assets for 285 (quote) + 20 (fee).
    let order_liquidated_user = state
        .facade
        .create_order(
            user: liquidated_user,
            base_amount: 3,
            base_asset_id: asset_id,
            quote_amount: -285,
            fee_amount: 3,
        );

    let mut order_liquidator_user_1 = state
        .facade
        .create_order(
            user: liquidator_user_1,
            base_amount: -2,
            base_asset_id: asset_id,
            quote_amount: 100,
            fee_amount: 0,
        );

    let mut order_liquidator_user_2 = state
        .facade
        .create_order(
            user: liquidator_user_2,
            base_amount: -1,
            base_asset_id: asset_id,
            quote_amount: 50,
            fee_amount: 0,
        );

    // Make trade.
    // User recieves 2 synthetic asset for 190 (quote) + 2 (fee).
    state
        .facade
        .trade(
            order_info_a: order_liquidated_user,
            order_info_b: order_liquidator_user_1,
            base: 2,
            quote: -190,
            fee_a: 2,
            fee_b: 0,
        );

    // User recieves 1 synthetic asset for 95 (quote) + 0 (fee).
    state
        .facade
        .trade(
            order_info_a: order_liquidated_user,
            order_info_b: order_liquidator_user_2,
            base: 1,
            quote: -95,
            fee_a: 0,
            fee_b: 0,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // liquidated User:    -287 + 3 * 100 = 13                3 * 100 * 0.01 = 3           4.3
    state
        .facade
        .validate_total_value(position_id: liquidated_user.position_id, expected_total_value: 13);
    state
        .facade
        .validate_total_risk(position_id: liquidated_user.position_id, expected_total_risk: 3);

    advance_time(10000);
    let mut new_funding_index = FundingIndex { value: 4 * FUNDING_SCALE };
    state
        .facade
        .funding_tick(
            funding_ticks: array![
                FundingTick { asset_id: asset_id, funding_index: new_funding_index },
            ]
                .span(),
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // liquidated User:    -299 + 3 * 100 = 1                 3 * 100 * 0.01 = 3           0.3
    state
        .facade
        .validate_total_value(position_id: liquidated_user.position_id, expected_total_value: 1);
    state
        .facade
        .validate_total_risk(position_id: liquidated_user.position_id, expected_total_risk: 3);

    assert(
        state.facade.is_liquidatable(position_id: liquidated_user.position_id),
        'user is not liquidatable',
    );

    order_liquidator_user_1 = state
        .facade
        .create_order(
            user: liquidator_user_1,
            base_amount: 1,
            base_asset_id: asset_id,
            quote_amount: -101,
            fee_amount: 1,
        );
    state
        .facade
        .liquidate(
            :liquidated_user,
            liquidator_order: order_liquidator_user_1,
            liquidated_base: -1,
            liquidated_quote: 101,
            liquidated_insurance_fee: 1,
            liquidator_fee: 1,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -199 + 2 * 10 = 1                  2 * 100 * 0.01 = 2           0.5
    state
        .facade
        .validate_total_value(position_id: liquidated_user.position_id, expected_total_value: 1);
    state
        .facade
        .validate_total_risk(position_id: liquidated_user.position_id, expected_total_risk: 2);

    assert(
        state.facade.is_liquidatable(position_id: liquidated_user.position_id),
        'user is not liquidatable',
    );

    order_liquidator_user_2 = state
        .facade
        .create_order(
            user: liquidator_user_2,
            base_amount: 2,
            base_asset_id: asset_id,
            quote_amount: -201,
            fee_amount: 1,
        );
    state
        .facade
        .liquidate(
            :liquidated_user,
            liquidator_order: order_liquidator_user_2,
            liquidated_base: -2,
            liquidated_quote: 201,
            liquidated_insurance_fee: 2,
            liquidator_fee: 1,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:        0 + 0 = 0                     0 * 100 * 0.01 = 0            0
    state
        .facade
        .validate_total_value(position_id: liquidated_user.position_id, expected_total_value: 0);
    state
        .facade
        .validate_total_risk(position_id: liquidated_user.position_id, expected_total_risk: 0);
}

#[test]
fn test_liquidate_after_price_tick() {
    // Setup.
    let risk_factor_data = RiskFactorTiers {
        tiers: array![100].span(), first_tier_boundary: MAX_U128, tier_size: 1,
    };
    // Create a custom asset configuration to test interesting risk factor scenarios.
    let synthetic_info = AssetInfoTrait::new(
        asset_name: 'BTC_1', :risk_factor_data, oracles_len: 1,
    );
    let asset_id = synthetic_info.asset_id;

    let mut state: FlowTestBase = FlowTestBaseTrait::new();

    state.facade.add_active_synthetic(synthetic_info: @synthetic_info, initial_price: 10);

    // Create users.
    let liquidated_user = state.new_user_with_position();
    let liquidator_user_1 = state.new_user_with_position();
    let liquidator_user_2 = state.new_user_with_position();

    // Deposit to users.
    let deposit_info_user_1 = state
        .facade
        .deposit(
            depositor: liquidator_user_1.account,
            position_id: liquidator_user_1.position_id,
            quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user_1);

    let deposit_info_user_2 = state
        .facade
        .deposit(
            depositor: liquidator_user_2.account,
            position_id: liquidator_user_2.position_id,
            quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user_2);

    // Create orders.
    // User willing to sell 3 synthetic assets for 66 (quote) - 6 (fee).
    let order_liquidated_user = state
        .facade
        .create_order(
            user: liquidated_user,
            base_amount: -3,
            base_asset_id: asset_id,
            quote_amount: 66,
            fee_amount: 6,
        );

    let mut order_liquidator_user_1 = state
        .facade
        .create_order(
            user: liquidator_user_1,
            base_amount: 2,
            base_asset_id: asset_id,
            quote_amount: -44,
            fee_amount: 4,
        );

    let mut order_liquidator_user_2 = state
        .facade
        .create_order(
            user: liquidator_user_2,
            base_amount: 1,
            base_asset_id: asset_id,
            quote_amount: -22,
            fee_amount: 2,
        );

    // Make trade.
    // User gives 2 synthetic asset for 44 (quote) - 2 (fee).
    state
        .facade
        .trade(
            order_info_a: order_liquidated_user,
            order_info_b: order_liquidator_user_1,
            base: -2,
            quote: 44,
            fee_a: 2,
            fee_b: 4,
        );

    // User recieves 1 synthetic asset for 22 (quote) - 1 (fee).
    state
        .facade
        .trade(
            order_info_a: order_liquidated_user,
            order_info_b: order_liquidator_user_2,
            base: -1,
            quote: 22,
            fee_a: 1,
            fee_b: 1,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // liquidated User:     63 - 3 * 10 = 33                 |-3 * 10 * 0.1| = 3           12
    state
        .facade
        .validate_total_value(position_id: liquidated_user.position_id, expected_total_value: 33);
    state
        .facade
        .validate_total_risk(position_id: liquidated_user.position_id, expected_total_risk: 3);

    state.facade.price_tick(asset_info: @synthetic_info, price: 20);

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // liquidated User:     63 - 3 * 20 = 3                  |-3 * 20 * 0.1| = 6            0.5
    state
        .facade
        .validate_total_value(position_id: liquidated_user.position_id, expected_total_value: 3);
    state
        .facade
        .validate_total_risk(position_id: liquidated_user.position_id, expected_total_risk: 6);

    assert(
        state.facade.is_liquidatable(position_id: liquidated_user.position_id),
        'user is not liquidatable',
    );

    order_liquidator_user_1 = state
        .facade
        .create_order(
            user: liquidator_user_1,
            base_amount: -2,
            base_asset_id: asset_id,
            quote_amount: 41,
            fee_amount: 1,
        );
    state
        .facade
        .liquidate(
            :liquidated_user,
            liquidator_order: order_liquidator_user_1,
            liquidated_base: 2,
            liquidated_quote: -41,
            liquidated_insurance_fee: 1,
            liquidator_fee: 1,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // liquidated User:      21 - 1 * 20 = 1                  |-1 * 20 * 0.1| = 2          0.5
    state
        .facade
        .validate_total_value(position_id: liquidated_user.position_id, expected_total_value: 1);
    state
        .facade
        .validate_total_risk(position_id: liquidated_user.position_id, expected_total_risk: 2);

    assert(
        state.facade.is_liquidatable(position_id: liquidated_user.position_id),
        'user is not liquidatable',
    );

    order_liquidator_user_2 = state
        .facade
        .create_order(
            user: liquidator_user_2,
            base_amount: -1,
            base_asset_id: asset_id,
            quote_amount: 20,
            fee_amount: 1,
        );
    state
        .facade
        .liquidate(
            :liquidated_user,
            liquidator_order: order_liquidator_user_2,
            liquidated_base: 1,
            liquidated_quote: -20,
            liquidated_insurance_fee: 0,
            liquidator_fee: 1,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // Delevereged User:        1 + 0 = 1                     0 * 100 * 0.01 = 0            -
    state
        .facade
        .validate_total_value(position_id: liquidated_user.position_id, expected_total_value: 1);
    state
        .facade
        .validate_total_risk(position_id: liquidated_user.position_id, expected_total_risk: 0);
}

#[test]
fn test_flow_get_risk_factor() {
    // Setup.
    let risk_factor_data = RiskFactorTiers {
        tiers: array![10, 500, 1000].span(), first_tier_boundary: 2001, tier_size: 1000,
    };
    // Create a custom asset configuration to test interesting risk factor scenarios.
    let synthetic_info = AssetInfoTrait::new(asset_name: 'BTC', :risk_factor_data, oracles_len: 1);
    let asset_id = synthetic_info.asset_id;

    let mut state: FlowTestBase = FlowTestBaseTrait::new();

    state.facade.add_active_synthetic(synthetic_info: @synthetic_info, initial_price: 1000);

    // Create users.
    let user_1 = state.new_user_with_position();
    let user_2 = state.new_user_with_position();

    let deposit_info_user_1 = state
        .facade
        .deposit(
            depositor: user_1.account, position_id: user_1.position_id, quantized_amount: 10000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user_1);

    let deposit_info_user_2 = state
        .facade
        .deposit(
            depositor: user_2.account, position_id: user_2.position_id, quantized_amount: 10000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user_2);

    // Create orders.
    let order_user_1 = state
        .facade
        .create_order(
            user: user_1,
            base_amount: 10,
            base_asset_id: asset_id,
            quote_amount: -10000,
            fee_amount: 0,
        );
    let order_user_2 = state
        .facade
        .create_order(
            user: user_2,
            base_amount: -20,
            base_asset_id: asset_id,
            quote_amount: 20000,
            fee_amount: 0,
        );

    // Test:
    // No synthetic assets.
    state.facade.validate_total_risk(position_id: user_1.position_id, expected_total_risk: 0);

    // Partial fulfillment.
    state
        .facade
        .trade(
            order_info_a: order_user_1,
            order_info_b: order_user_2,
            base: 2,
            quote: -2000,
            fee_a: 0,
            fee_b: 0,
        );

    // 2000 * 1%.
    state.facade.validate_total_risk(position_id: user_1.position_id, expected_total_risk: 20);

    // Partial fulfillment.
    state
        .facade
        .trade(
            order_info_a: order_user_1,
            order_info_b: order_user_2,
            base: 1,
            quote: -1000,
            fee_a: 0,
            fee_b: 0,
        );

    // index = (3000 - 2001)/1000 = 1;
    // 3000 * 50%.
    state.facade.validate_total_risk(position_id: user_1.position_id, expected_total_risk: 1500);

    // Partial fulfillment.
    state
        .facade
        .trade(
            order_info_a: order_user_1,
            order_info_b: order_user_2,
            base: 1,
            quote: -1000,
            fee_a: 0,
            fee_b: 0,
        );

    // index = (4000 - 2001)/1000 = 2;
    // 4000 * 100%.
    state.facade.validate_total_risk(position_id: user_1.position_id, expected_total_risk: 4000);

    // Partial fulfillment.
    state
        .facade
        .trade(
            order_info_a: order_user_1,
            order_info_b: order_user_2,
            base: 5,
            quote: -5000,
            fee_a: 0,
            fee_b: 0,
        );
    // index = (9000 - 2001)/1000 = 7 > 3; (last index)
    // 9000 * 100%.
    state.facade.validate_total_risk(position_id: user_1.position_id, expected_total_risk: 9000);
}

#[test]
fn test_transfer() {
    // Setup.
    let mut state: FlowTestBase = FlowTestBaseTrait::new();

    // Create users.
    let user_1 = state.new_user_with_position();
    let user_2 = state.new_user_with_position();
    let user_3 = state.new_user_with_position();

    // Deposit to users.
    let deposit_info_user_1 = state
        .facade
        .deposit(
            depositor: user_1.account, position_id: user_1.position_id, quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user_1);

    // Transfer.
    let mut transfer_info = state
        .facade
        .transfer_request(sender: user_1, recipient: user_2, amount: 40000);
    state.facade.transfer(:transfer_info);

    transfer_info = state.facade.transfer_request(sender: user_1, recipient: user_3, amount: 20000);
    state.facade.transfer(:transfer_info);

    transfer_info = state.facade.transfer_request(sender: user_2, recipient: user_3, amount: 10000);
    state.facade.transfer(:transfer_info);

    transfer_info = state.facade.transfer_request(sender: user_2, recipient: user_1, amount: 5000);
    state.facade.transfer(:transfer_info);

    transfer_info = state.facade.transfer_request(sender: user_1, recipient: user_2, amount: 30000);
    state.facade.transfer(:transfer_info);

    //                 COLLATERAL
    // User 1:           15,000
    // User 2:           55,000
    // User 3:           30,000

    // Withdraw.
    let mut withdraw_info = state.facade.withdraw_request(user: user_1, amount: 15000);
    state.facade.withdraw(:withdraw_info);

    withdraw_info = state.facade.withdraw_request(user: user_2, amount: 15000);
    state.facade.withdraw(:withdraw_info);

    withdraw_info = state.facade.withdraw_request(user: user_2, amount: 10000);
    state.facade.withdraw(:withdraw_info);

    withdraw_info = state.facade.withdraw_request(user: user_2, amount: 30000);
    state.facade.withdraw(:withdraw_info);

    withdraw_info = state.facade.withdraw_request(user: user_3, amount: 30000);
    state.facade.withdraw(:withdraw_info);
}

#[test]
fn test_withdraw_with_owner() {
    // Setup.
    let mut state: FlowTestBase = FlowTestBaseTrait::new();

    // Create users.
    let user_1 = state.new_user_with_position();

    // Deposit to users.
    let deposit_info_user_1 = state
        .facade
        .deposit(
            depositor: user_1.account, position_id: user_1.position_id, quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user_1);

    // Withdraw.
    let mut withdraw_info = state.facade.withdraw_request(user: user_1, amount: 15000);
    state.facade.withdraw(:withdraw_info);
}

#[test]
#[should_panic(expected: 'CALLER_IS_NOT_OWNER_ACCOUNT')]
fn test_withdraw_with_owner_fails_if_not_caller() {
    // Setup.
    let mut state: FlowTestBase = FlowTestBaseTrait::new();

    // Create users.
    let user_1 = state.new_user_with_position();
    let user_2 = state.new_user_with_position();

    // Deposit to users.
    let deposit_info_user_1 = state
        .facade
        .deposit(
            depositor: user_1.account, position_id: user_1.position_id, quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user_1);

    // Withdraw.
    let mut withdraw_info = state
        .facade
        .withdraw_request_with_caller(
            user: user_1, asset_id: state.facade.collateral_id, amount: 15000, caller: user_2,
        );
    state.facade.withdraw(:withdraw_info);
}

#[test]
fn test_transfer_withdraw_with_negative_collateral() {
    // Setup.
    let risk_factor_data = RiskFactorTiers {
        tiers: array![10].span(), first_tier_boundary: MAX_U128, tier_size: 1,
    };
    // Create a custom asset configuration to test interesting risk factor scenarios.
    let synthetic_info = AssetInfoTrait::new(
        asset_name: 'BTC_1', :risk_factor_data, oracles_len: 1,
    );
    let asset_id = synthetic_info.asset_id;

    let mut state: FlowTestBase = FlowTestBaseTrait::new();

    state.facade.add_active_synthetic(synthetic_info: @synthetic_info, initial_price: 100);

    // Create users.
    let user_1 = state.new_user_with_position();
    let user_2 = state.new_user_with_position();

    // Deposit to users.
    let deposit_info_user_2 = state
        .facade
        .deposit(
            depositor: user_2.account, position_id: user_2.position_id, quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user_2);

    // Create orders.
    let order_user_1 = state
        .facade
        .create_order(
            user: user_1, base_amount: 1, base_asset_id: asset_id, quote_amount: -5, fee_amount: 0,
        );

    let order_user_2 = state
        .facade
        .create_order(
            user: user_2, base_amount: -1, base_asset_id: asset_id, quote_amount: 5, fee_amount: 0,
        );

    // Make trade.
    state
        .facade
        .trade(
            order_info_a: order_user_1,
            order_info_b: order_user_2,
            base: 1,
            quote: -5,
            fee_a: 0,
            fee_b: 0,
        );

    // Transfer.
    let transfer_info = state
        .facade
        .transfer_request(sender: user_1, recipient: user_2, amount: 20);
    state.facade.transfer(:transfer_info);

    //                    TV                                  TR                 TV / TR
    //         COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // User 1:      -25 + 1 * 100 = 75                 1 * 100 * 0.01 = 1          75
    state.facade.validate_total_value(position_id: user_1.position_id, expected_total_value: 75);
    state.facade.validate_total_risk(position_id: user_1.position_id, expected_total_risk: 1);

    // Withdraw.
    let withdraw_info = state.facade.withdraw_request(user: user_1, amount: 70);
    state.facade.withdraw(:withdraw_info);

    //                    TV                                  TR                 TV / TR
    //         COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // User 1:      -95 + 1 * 100 = 5                  1 * 100 * 0.01 = 1          5
    state.facade.validate_total_value(position_id: user_1.position_id, expected_total_value: 5);
    state.facade.validate_total_risk(position_id: user_1.position_id, expected_total_risk: 1);
}

#[test]
fn test_withdraw_spot_collateral() {
    // Setup.
    let risk_factor_data = RiskFactorTiers {
        tiers: array![10].span(), first_tier_boundary: MAX_U128, tier_size: 1,
    };
    let mut state: FlowTestBase = FlowTestBaseTrait::new();

    // Create a custom asset configuration.
    let token = snforge_std::Token::STRK;
    let erc20_contract_address = token.contract_address();
    let asset_info = AssetInfoTrait::new_collateral(
        asset_name: 'COL', :risk_factor_data, oracles_len: 1, :erc20_contract_address,
    );
    let asset_id = asset_info.asset_id;
    state.facade.add_active_collateral(asset_info: @asset_info, initial_price: 100);

    // Create users.
    let user = state.new_user_with_position();
    snforge_std::set_balance(target: user.account.address, new_balance: 5000000, :token);

    // Deposit to users.
    let deposit_info_user = state
        .facade
        .deposit_spot(
            depositor: user.account,
            :asset_id,
            position_id: user.position_id,
            quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user);

    // Withdraw.
    let mut withdraw_info = state.facade.withdraw_spot_request(:user, :asset_id, amount: 15000);
    state.facade.withdraw(:withdraw_info);
}

#[test]
fn test_reduce_synthetic() {
    // Setup.
    let risk_factor_data = RiskFactorTiers {
        tiers: array![30].span(), first_tier_boundary: MAX_U128, tier_size: 1,
    };
    // Create a custom asset configuration to test interesting risk factor scenarios.
    let synthetic_info = AssetInfoTrait::new(
        asset_name: 'BTC_1', :risk_factor_data, oracles_len: 1,
    );
    let asset_id = synthetic_info.asset_id;

    let mut state: FlowTestBase = FlowTestBaseTrait::new();

    state.facade.add_active_synthetic(synthetic_info: @synthetic_info, initial_price: 100);

    // Create users.
    let user_1 = state.new_user_with_position();
    let user_2 = state.new_user_with_position();

    // Deposit to users.
    let deposit_info_user_2 = state
        .facade
        .deposit(
            depositor: user_2.account, position_id: user_2.position_id, quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user_2);

    // Create orders.
    let order_user_1 = state
        .facade
        .create_order(
            user: user_1, base_amount: 1, base_asset_id: asset_id, quote_amount: -95, fee_amount: 0,
        );

    let mut order_user_2 = state
        .facade
        .create_order(
            user: user_2, base_amount: -1, base_asset_id: asset_id, quote_amount: 95, fee_amount: 0,
        );

    // Make trade.
    state
        .facade
        .trade(
            order_info_a: order_user_1,
            order_info_b: order_user_2,
            base: 1,
            quote: -95,
            fee_a: 0,
            fee_b: 0,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // User 1:             -95 + 1 * 100 = 5                 |1 * 100 * 0.03| = 3          1.6
    state.facade.validate_total_value(position_id: user_1.position_id, expected_total_value: 5);
    state.facade.validate_total_risk(position_id: user_1.position_id, expected_total_risk: 3);

    advance_time(10000);
    let mut new_funding_index = FundingIndex { value: 3 * FUNDING_SCALE };
    state
        .facade
        .funding_tick(
            funding_ticks: array![
                FundingTick { asset_id: asset_id, funding_index: new_funding_index },
            ]
                .span(),
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // User 1:             -98 + 1 * 100 = 2                 |1 * 100 * 0.03| = 3          0.6
    state.facade.validate_total_value(position_id: user_1.position_id, expected_total_value: 2);
    state.facade.validate_total_risk(position_id: user_1.position_id, expected_total_risk: 3);

    assert(
        state.facade.is_liquidatable(position_id: user_1.position_id), 'user is not liquidatable',
    );

    state.facade.deactivate_synthetic(synthetic_id: asset_id);
    state
        .facade
        .reduce_asset_position(
            position_id_a: user_1.position_id,
            position_id_b: user_2.position_id,
            base_asset_id: asset_id,
            base_amount_a: -1,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // User 1:                2 + 0 = 2                      |0 * 100 * 0.03| = 0            -
    state.facade.validate_total_value(position_id: user_1.position_id, expected_total_value: 2);
    state.facade.validate_total_risk(position_id: user_1.position_id, expected_total_risk: 0);
}

/// The following test checks the transitions between healthy, liquidatable and deleveragable by
/// using funding tick and price tick.
/// We do so in the following way:
/// User is healthy -> Funding tick occurs -> User is liquidatable -> Funding tick occurs -> User is
/// deleveragable -> Price tick occurs -> User is liquidatable -> Price tick occurs -> User is
/// healthy -> Price tick occurs -> User is deleveragable -> Funding tick occurs -> User is healthy.
#[test]
fn test_status_change_healthy_liquidatable_deleveragable() {
    // Setup.
    let risk_factor_data = RiskFactorTiers {
        tiers: array![10].span(), first_tier_boundary: MAX_U128, tier_size: 1,
    };
    // Create a custom asset configuration to test interesting risk factor scenarios.
    let synthetic_info = AssetInfoTrait::new(
        asset_name: 'BTC_1', :risk_factor_data, oracles_len: 1,
    );
    let asset_id = synthetic_info.asset_id;

    let mut state: FlowTestBase = FlowTestBaseTrait::new();

    state.facade.add_active_synthetic(synthetic_info: @synthetic_info, initial_price: 100);

    // Create users.
    let primary_user = state.new_user_with_position();
    let support_user = state.new_user_with_position();

    // Deposit to users.
    let deposit_info_support_user = state
        .facade
        .deposit(
            depositor: support_user.account,
            position_id: support_user.position_id,
            quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_support_user);

    // Create orders.
    let mut order_primary_user = state
        .facade
        .create_order(
            user: primary_user,
            base_amount: 2,
            base_asset_id: asset_id,
            quote_amount: -195,
            fee_amount: 0,
        );

    let mut order_support_user = state
        .facade
        .create_order(
            user: support_user,
            base_amount: -2,
            base_asset_id: asset_id,
            quote_amount: 195,
            fee_amount: 0,
        );

    // Make trades.
    state
        .facade
        .trade(
            order_info_a: order_primary_user,
            order_info_b: order_support_user,
            base: 2,
            quote: -195,
            fee_a: 0,
            fee_b: 0,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -195 + 2 * 100 = 5                 2 * 100 * 0.01 = 2           2.5
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 5);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 2);

    advance_time(10000);
    let mut new_funding_index = FundingIndex { value: 2 * FUNDING_SCALE };
    state
        .facade
        .funding_tick(
            funding_ticks: array![
                FundingTick { asset_id: asset_id, funding_index: new_funding_index },
            ]
                .span(),
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -199 + 2 * 100 = 1                 2 * 100 * 0.01 = 2           0.5
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 1);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 2);

    assert(
        state.facade.is_liquidatable(position_id: primary_user.position_id),
        'user is not liquidatable',
    );

    advance_time(10000);
    new_funding_index = FundingIndex { value: 4 * FUNDING_SCALE };
    state
        .facade
        .funding_tick(
            funding_ticks: array![
                FundingTick { asset_id: asset_id, funding_index: new_funding_index },
            ]
                .span(),
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -203 + 2 * 100 = -3                 2 * 100 * 0.01 = 2         -1.5
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: -3);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 2);

    assert(
        state.facade.is_deleveragable(position_id: primary_user.position_id),
        'user is not deleveragable',
    );

    state.facade.price_tick(asset_info: @synthetic_info, price: 102);
    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -203 + 2 * 102 = 1                 2 * 102 * 0.01 = 2           0.5
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 1);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 2);

    assert(
        state.facade.is_liquidatable(position_id: primary_user.position_id),
        'user is not liquidatable',
    );

    state.facade.price_tick(asset_info: @synthetic_info, price: 103);
    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -203 + 2 * 103 = 3                 2 * 103 * 0.01 = 2           1.5
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 3);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 2);

    // Create orders.
    order_primary_user = state
        .facade
        .create_order(
            user: primary_user,
            base_amount: 1,
            base_asset_id: asset_id,
            quote_amount: -100,
            fee_amount: 0,
        );

    order_support_user = state
        .facade
        .create_order(
            user: support_user,
            base_amount: -1,
            base_asset_id: asset_id,
            quote_amount: 100,
            fee_amount: 0,
        );

    // Make trades.
    state
        .facade
        .trade(
            order_info_a: order_primary_user,
            order_info_b: order_support_user,
            base: 1,
            quote: -100,
            fee_a: 0,
            fee_b: 0,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -303 + 3 * 100 = 6                 3 * 103 * 0.01 = 3            2
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 6);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 3);

    state.facade.price_tick(asset_info: @synthetic_info, price: 100);
    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -303 + 3 * 100 = -3                 3 * 100 * 0.01 = 3          -1
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: -3);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 3);

    assert(
        state.facade.is_deleveragable(position_id: primary_user.position_id),
        'user is not deleveragable',
    );

    advance_time(10000);
    new_funding_index = FundingIndex { value: FUNDING_SCALE };
    state
        .facade
        .funding_tick(
            funding_ticks: array![
                FundingTick { asset_id: asset_id, funding_index: new_funding_index },
            ]
                .span(),
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -294 + 3 * 100 = 6                 3 * 100 * 0.01 = 3            2
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 6);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 3);

    // Create orders.
    order_primary_user = state
        .facade
        .create_order(
            user: primary_user,
            base_amount: 1,
            base_asset_id: asset_id,
            quote_amount: -100,
            fee_amount: 0,
        );

    order_support_user = state
        .facade
        .create_order(
            user: support_user,
            base_amount: -1,
            base_asset_id: asset_id,
            quote_amount: 100,
            fee_amount: 0,
        );

    // Make trades.
    state
        .facade
        .trade(
            order_info_a: order_primary_user,
            order_info_b: order_support_user,
            base: 1,
            quote: -100,
            fee_a: 0,
            fee_b: 0,
        );
}

#[test]
fn test_status_change_by_deposit() {
    // Setup.
    let risk_factor_data = RiskFactorTiers {
        tiers: array![10].span(), first_tier_boundary: MAX_U128, tier_size: 1,
    };
    let synthetic_info = AssetInfoTrait::new(
        asset_name: 'BTC_1', :risk_factor_data, oracles_len: 1,
    );
    let asset_id = synthetic_info.asset_id;

    let mut state: FlowTestBase = FlowTestBaseTrait::new();

    state.facade.add_active_synthetic(synthetic_info: @synthetic_info, initial_price: 100);

    // Create users.
    let primary_user = state.new_user_with_position();
    let support_user = state.new_user_with_position();

    // Deposit to users.
    let mut deposit_info_user = state
        .facade
        .deposit(
            depositor: support_user.account,
            position_id: support_user.position_id,
            quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user);

    // Create orders.
    let mut order_primary_user = state
        .facade
        .create_order(
            user: primary_user,
            base_amount: 2,
            base_asset_id: asset_id,
            quote_amount: -195,
            fee_amount: 0,
        );

    let mut order_support_user = state
        .facade
        .create_order(
            user: support_user,
            base_amount: -2,
            base_asset_id: asset_id,
            quote_amount: 195,
            fee_amount: 0,
        );

    // Make trades.
    state
        .facade
        .trade(
            order_info_a: order_primary_user,
            order_info_b: order_support_user,
            base: 2,
            quote: -195,
            fee_a: 0,
            fee_b: 0,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -195 + 2 * 100 = 5                 2 * 100 * 0.01 = 2           2.5
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 5);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 2);

    advance_time(10000);
    let mut new_funding_index = FundingIndex { value: 4 * FUNDING_SCALE };
    state
        .facade
        .funding_tick(
            funding_ticks: array![
                FundingTick { asset_id: asset_id, funding_index: new_funding_index },
            ]
                .span(),
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -203 + 2 * 100 = -3                 2 * 100 * 0.01 = 2         -1.5
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: -3);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 2);

    assert(
        state.facade.is_deleveragable(position_id: primary_user.position_id),
        'user is not deleveragable',
    );

    deposit_info_user = state
        .facade
        .deposit(
            depositor: primary_user.account,
            position_id: primary_user.position_id,
            quantized_amount: 4,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user);

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -199 + 2 * 100 = 1                 2 * 100 * 0.01 = 2           0.5

    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 1);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 2);

    assert(
        state.facade.is_liquidatable(position_id: primary_user.position_id),
        'user is not liquidatable',
    );

    deposit_info_user = state
        .facade
        .deposit(
            depositor: primary_user.account,
            position_id: primary_user.position_id,
            quantized_amount: 1,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user);

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -198 + 2 * 100 = 2                 2 * 100 * 0.01 = 2            1
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 2);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 2);

    // Create orders.
    let mut order_primary_user = state
        .facade
        .create_order(
            user: primary_user,
            base_amount: -1,
            base_asset_id: asset_id,
            quote_amount: 100,
            fee_amount: 0,
        );

    let mut order_support_user = state
        .facade
        .create_order(
            user: support_user,
            base_amount: 1,
            base_asset_id: asset_id,
            quote_amount: -100,
            fee_amount: 0,
        );

    // Make trades.
    state
        .facade
        .trade(
            order_info_a: order_primary_user,
            order_info_b: order_support_user,
            base: -1,
            quote: 100,
            fee_a: 0,
            fee_b: 0,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -98 + 1 * 100 = 2                  1 * 100 * 0.01 = 1            2
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 2);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 1);
}

#[test]
fn test_status_change_by_transfer() {
    // Setup.
    let risk_factor_data = RiskFactorTiers {
        tiers: array![10].span(), first_tier_boundary: MAX_U128, tier_size: 1,
    };
    let synthetic_info = AssetInfoTrait::new(
        asset_name: 'BTC_1', :risk_factor_data, oracles_len: 1,
    );
    let asset_id = synthetic_info.asset_id;
    let mut state: FlowTestBase = FlowTestBaseTrait::new();
    state.facade.add_active_synthetic(synthetic_info: @synthetic_info, initial_price: 100);
    // Create users.
    let primary_user = state.new_user_with_position();
    let support_user = state.new_user_with_position();
    // Deposit to users.
    let deposit_info_user = state
        .facade
        .deposit(
            depositor: support_user.account,
            position_id: support_user.position_id,
            quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user);
    // Create orders.
    let mut order_primary_user = state
        .facade
        .create_order(
            user: primary_user,
            base_amount: 2,
            base_asset_id: asset_id,
            quote_amount: -195,
            fee_amount: 0,
        );
    let mut order_support_user = state
        .facade
        .create_order(
            user: support_user,
            base_amount: -2,
            base_asset_id: asset_id,
            quote_amount: 195,
            fee_amount: 0,
        );
    // Make trades.
    state
        .facade
        .trade(
            order_info_a: order_primary_user,
            order_info_b: order_support_user,
            base: 2,
            quote: -195,
            fee_a: 0,
            fee_b: 0,
        );
    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -195 + 2 * 100 = 5                 2 * 100 * 0.01 = 2           2.5
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 5);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 2);
    advance_time(10000);
    let mut new_funding_index = FundingIndex { value: 4 * FUNDING_SCALE };
    state
        .facade
        .funding_tick(
            funding_ticks: array![
                FundingTick { asset_id: asset_id, funding_index: new_funding_index },
            ]
                .span(),
        );
    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -203 + 2 * 100 = -3                 2 * 100 * 0.01 = 2         -1.5
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: -3);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 2);

    assert(
        state.facade.is_deleveragable(position_id: primary_user.position_id),
        'user is not deleveragable',
    );

    let mut transfer_info = state
        .facade
        .transfer_request(sender: support_user, recipient: primary_user, amount: 4);
    state.facade.transfer(:transfer_info);
    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -199 + 2 * 100 = 1                 2 * 100 * 0.01 = 2           0.5
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 1);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 2);

    assert(
        state.facade.is_liquidatable(position_id: primary_user.position_id),
        'user is not liquidatable',
    );

    transfer_info = state
        .facade
        .transfer_request(sender: support_user, recipient: primary_user, amount: 1);
    state.facade.transfer(:transfer_info);

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -198 + 2 * 100 = 2                 2 * 100 * 0.01 = 2            1
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 2);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 2);

    // Create orders.
    let mut order_primary_user = state
        .facade
        .create_order(
            user: primary_user,
            base_amount: -1,
            base_asset_id: asset_id,
            quote_amount: 100,
            fee_amount: 0,
        );

    let mut order_support_user = state
        .facade
        .create_order(
            user: support_user,
            base_amount: 1,
            base_asset_id: asset_id,
            quote_amount: -100,
            fee_amount: 0,
        );

    // Make trades.
    state
        .facade
        .trade(
            order_info_a: order_primary_user,
            order_info_b: order_support_user,
            base: -1,
            quote: 100,
            fee_a: 0,
            fee_b: 0,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -98 + 1 * 100 = 2                  1 * 100 * 0.01 = 1            2
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 2);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 1);
}

#[test]
fn test_status_change_by_trade() {
    // Setup.
    let risk_factor_data = RiskFactorTiers {
        tiers: array![10].span(), first_tier_boundary: MAX_U128, tier_size: 1,
    };
    let synthetic_info = AssetInfoTrait::new(
        asset_name: 'BTC_1', :risk_factor_data, oracles_len: 1,
    );
    let asset_id = synthetic_info.asset_id;
    let mut state: FlowTestBase = FlowTestBaseTrait::new();
    state.facade.add_active_synthetic(synthetic_info: @synthetic_info, initial_price: 100);
    // Create users.
    let primary_user = state.new_user_with_position();
    let support_user = state.new_user_with_position();
    // Deposit to users.
    let deposit_info_user = state
        .facade
        .deposit(
            depositor: support_user.account,
            position_id: support_user.position_id,
            quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user);
    // Create orders.
    let mut order_primary_user = state
        .facade
        .create_order(
            user: primary_user,
            base_amount: 6,
            base_asset_id: asset_id,
            quote_amount: -594,
            fee_amount: 0,
        );
    let mut order_support_user = state
        .facade
        .create_order(
            user: support_user,
            base_amount: -6,
            base_asset_id: asset_id,
            quote_amount: 594,
            fee_amount: 0,
        );
    // Make trades.
    state
        .facade
        .trade(
            order_info_a: order_primary_user,
            order_info_b: order_support_user,
            base: 6,
            quote: -594,
            fee_a: 0,
            fee_b: 0,
        );
    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -594 + 6 * 100 = 6                 6 * 100 * 0.01 = 6            1
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 6);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 6);
    advance_time(10000);
    let mut new_funding_index = FundingIndex { value: 2 * FUNDING_SCALE };
    state
        .facade
        .funding_tick(
            funding_ticks: array![
                FundingTick { asset_id: asset_id, funding_index: new_funding_index },
            ]
                .span(),
        );
    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -606 + 6 * 100 = -6                 6 * 100 * 0.01 = 6          -1
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: -6);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 6);

    assert(
        state.facade.is_deleveragable(position_id: primary_user.position_id),
        'user is not deleveragable',
    );

    // Create orders.
    let mut order_primary_user = state
        .facade
        .create_order(
            user: primary_user,
            base_amount: -1,
            base_asset_id: asset_id,
            quote_amount: 104,
            fee_amount: 0,
        );
    let mut order_support_user = state
        .facade
        .create_order(
            user: support_user,
            base_amount: 1,
            base_asset_id: asset_id,
            quote_amount: -104,
            fee_amount: 0,
        );
    // Make trades.
    state
        .facade
        .trade(
            order_info_a: order_primary_user,
            order_info_b: order_support_user,
            base: -1,
            quote: 104,
            fee_a: 0,
            fee_b: 0,
        );
    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -502 + 5 * 100 = -2                5 * 100 * 0.01 = 5          -0.4
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: -2);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 5);

    assert(
        state.facade.is_deleveragable(position_id: primary_user.position_id),
        'user is not deleveragable',
    );

    // Create orders.
    let mut order_primary_user = state
        .facade
        .create_order(
            user: primary_user,
            base_amount: -1,
            base_asset_id: asset_id,
            quote_amount: 102,
            fee_amount: 0,
        );
    let mut order_support_user = state
        .facade
        .create_order(
            user: support_user,
            base_amount: 1,
            base_asset_id: asset_id,
            quote_amount: -102,
            fee_amount: 0,
        );
    // Make trades.
    state
        .facade
        .trade(
            order_info_a: order_primary_user,
            order_info_b: order_support_user,
            base: -1,
            quote: 102,
            fee_a: 0,
            fee_b: 0,
        );
    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -400 + 4 * 100 = 0                 4 * 100 * 0.01 = 4            0
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 0);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 4);
    // Create orders.
    let mut order_primary_user = state
        .facade
        .create_order(
            user: primary_user,
            base_amount: -1,
            base_asset_id: asset_id,
            quote_amount: 100,
            fee_amount: 0,
        );
    let mut order_support_user = state
        .facade
        .create_order(
            user: support_user,
            base_amount: 1,
            base_asset_id: asset_id,
            quote_amount: -100,
            fee_amount: 0,
        );
    // Make trades.
    state
        .facade
        .trade(
            order_info_a: order_primary_user,
            order_info_b: order_support_user,
            base: -1,
            quote: 100,
            fee_a: 0,
            fee_b: 0,
        );
    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -300 + 3 * 100 = 0                 3 * 100 * 0.01 = 3            0
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 0);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 3);
    // Create orders.
    let mut order_primary_user = state
        .facade
        .create_order(
            user: primary_user,
            base_amount: -1,
            base_asset_id: asset_id,
            quote_amount: 101,
            fee_amount: 0,
        );
    let mut order_support_user = state
        .facade
        .create_order(
            user: support_user,
            base_amount: 1,
            base_asset_id: asset_id,
            quote_amount: -101,
            fee_amount: 0,
        );
    // Make trades.
    state
        .facade
        .trade(
            order_info_a: order_primary_user,
            order_info_b: order_support_user,
            base: -1,
            quote: 101,
            fee_a: 0,
            fee_b: 0,
        );
    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -199 + 2 * 100 = 1                 2 * 100 * 0.01 = 2           0.5
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 1);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 2);

    assert(
        state.facade.is_liquidatable(position_id: primary_user.position_id),
        'user is not liquidatable',
    );

    // Create orders.
    let mut order_primary_user = state
        .facade
        .create_order(
            user: primary_user,
            base_amount: -2,
            base_asset_id: asset_id,
            quote_amount: 199,
            fee_amount: 0,
        );
    let mut order_support_user = state
        .facade
        .create_order(
            user: support_user,
            base_amount: 2,
            base_asset_id: asset_id,
            quote_amount: -199,
            fee_amount: 0,
        );
    // Make trades.
    state
        .facade
        .trade(
            order_info_a: order_primary_user,
            order_info_b: order_support_user,
            base: -2,
            quote: 199,
            fee_a: 0,
            fee_b: 0,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:     0 + 0 * 100 = 0                 0 * 100 * 0.01 = 0            -
    state
        .facade
        .validate_total_value(position_id: primary_user.position_id, expected_total_value: 0);
    state.facade.validate_total_risk(position_id: primary_user.position_id, expected_total_risk: 0);
}

#[test]
#[should_panic(expected: 'SYNTHETIC_EXPIRED_PRICE')]
fn test_late_funding() {
    // Setup.
    let risk_factor_data = RiskFactorTiers {
        tiers: array![10].span(), first_tier_boundary: MAX_U128, tier_size: 1,
    };
    // Create a custom asset configuration to test interesting risk factor scenarios.
    let synthetic_info = AssetInfoTrait::new(
        asset_name: 'BTC_1', :risk_factor_data, oracles_len: 1,
    );
    let asset_id = synthetic_info.asset_id;

    let mut state: FlowTestBase = FlowTestBaseTrait::new();

    state.facade.add_active_synthetic(synthetic_info: @synthetic_info, initial_price: 100);

    advance_time(100000);
    let mut new_funding_index = FundingIndex { value: FUNDING_SCALE };
    state
        .facade
        .funding_tick(
            funding_ticks: array![
                FundingTick { asset_id: asset_id, funding_index: new_funding_index },
            ]
                .span(),
        );
}

#[test]
#[should_panic(expected: 'INVALID_BASE_CHANGE')]
fn test_liquidate_change_sign() {
    // Setup.
    let risk_factor_data = RiskFactorTiers {
        tiers: array![10].span(), first_tier_boundary: MAX_U128, tier_size: 1,
    };
    let synthetic_info = AssetInfoTrait::new(
        asset_name: 'BTC_1', :risk_factor_data, oracles_len: 1,
    );
    let asset_id = synthetic_info.asset_id;

    let mut state: FlowTestBase = FlowTestBaseTrait::new();

    state.facade.add_active_synthetic(synthetic_info: @synthetic_info, initial_price: 103);

    // Create users.
    let user_1 = state.new_user_with_position();
    let user_2 = state.new_user_with_position();

    // Deposit to users.
    let deposit_info_user_2 = state
        .facade
        .deposit(
            depositor: user_2.account, position_id: user_2.position_id, quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user_2);

    // Create orders.
    let mut order_user_1 = state
        .facade
        .create_order(
            user: user_1,
            base_amount: 3,
            base_asset_id: asset_id,
            quote_amount: -306,
            fee_amount: 0,
        );

    let mut order_user_2 = state
        .facade
        .create_order(
            user: user_2,
            base_amount: -3,
            base_asset_id: asset_id,
            quote_amount: 305,
            fee_amount: 0,
        );

    // Make trade.
    state
        .facade
        .trade(
            order_info_a: order_user_1,
            order_info_b: order_user_2,
            base: 3,
            quote: -305,
            fee_a: 0,
            fee_b: 0,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // User 1:             -305 + 3 * 103 = 3                  3 * 103 * 0.01 = 3.09        1.29
    state.facade.validate_total_value(position_id: user_1.position_id, expected_total_value: 4);
    state.facade.validate_total_risk(position_id: user_1.position_id, expected_total_risk: 3);

    // Price tick.
    state.facade.price_tick(asset_info: @synthetic_info, price: 100);

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // User 1:             -305 + 3 * 100 = -5                 3 * 100 * 0.01 = 3          -1.66

    state.facade.validate_total_value(position_id: user_1.position_id, expected_total_value: -5);
    state.facade.validate_total_risk(position_id: user_1.position_id, expected_total_risk: 3);

    // Liquidate.
    order_user_2 = state
        .facade
        .create_order(
            user: user_2,
            base_amount: 5,
            base_asset_id: asset_id,
            quote_amount: -505,
            fee_amount: 0,
        );
    state
        .facade
        .liquidate(
            liquidated_user: user_1,
            liquidator_order: order_user_2,
            liquidated_base: -5,
            liquidated_quote: 505,
            liquidated_insurance_fee: 0,
            liquidator_fee: 0,
        );
}

#[test]
fn test_funding_index_rounding() {
    let risk_factor_data = RiskFactorTiers {
        tiers: array![10].span(), first_tier_boundary: MAX_U128, tier_size: 1,
    };
    let synthetic_info = AssetInfoTrait::new(asset_name: 'BTC', :risk_factor_data, oracles_len: 1);
    let asset_id = synthetic_info.asset_id;

    let mut state: FlowTestBase = FlowTestBaseTrait::new();

    state.facade.add_active_synthetic(synthetic_info: @synthetic_info, initial_price: 100);

    let user_1 = state.new_user_with_position();
    let user_2 = state.new_user_with_position();

    let deposit_info_user_1 = state
        .facade
        .deposit(
            depositor: user_1.account, position_id: user_1.position_id, quantized_amount: 1100,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user_1);

    let deposit_info_user_2 = state
        .facade
        .deposit(depositor: user_2.account, position_id: user_2.position_id, quantized_amount: 900);
    state.facade.process_deposit(deposit_info: deposit_info_user_2);

    let order_1 = state
        .facade
        .create_order(
            user: user_1,
            base_amount: 1,
            base_asset_id: asset_id,
            quote_amount: -100,
            fee_amount: 0,
        );

    let order_2 = state
        .facade
        .create_order(
            user: user_2,
            base_amount: -1,
            base_asset_id: asset_id,
            quote_amount: 100,
            fee_amount: 0,
        );

    state
        .facade
        .trade(
            order_info_a: order_1, order_info_b: order_2, base: 1, quote: -100, fee_a: 0, fee_b: 0,
        );

    // Collateral balance before is 1000 each.
    state.facade.validate_collateral_balance(user_1.position_id, 1000_i64.into());
    state.facade.validate_collateral_balance(user_2.position_id, 1000_i64.into());

    // funding tick of half
    advance_time(10000);
    let mut new_funding_index = FundingIndex { value: FUNDING_SCALE / 2 };
    state
        .facade
        .funding_tick(
            funding_ticks: array![FundingTick { asset_id, funding_index: new_funding_index }]
                .span(),
        );

    /// Longer gets decremented by half, which rounds down to -1. Shorter gets incremented by half,
    /// which rounds down to 0.
    state.facade.validate_collateral_balance(user_1.position_id, 999_i64.into());
    state.facade.validate_collateral_balance(user_2.position_id, 1000_i64.into());

    // funding tick of minus half
    advance_time(10000);
    let mut new_funding_index = FundingIndex { value: -FUNDING_SCALE / 2 };
    state
        .facade
        .funding_tick(
            funding_ticks: array![FundingTick { asset_id, funding_index: new_funding_index }]
                .span(),
        );

    /// Longer gets incremented by half, which rounds down to 0. Shorter gets decremented by half,
    /// which rounds down to -1.
    state.facade.validate_collateral_balance(user_1.position_id, 1000_i64.into());
    state.facade.validate_collateral_balance(user_2.position_id, 999_i64.into());
}

#[test]
#[should_panic(
    expected: "POSITION_IS_NOT_FAIR_DELEVERAGE position_id: PositionId { value: 101 } TV before -2, TR before 2, TV after 2, TR after 0",
)]
fn test_unfair_deleverage() {
    // Setup.
    let risk_factor_data = RiskFactorTiers {
        tiers: array![10].span(), first_tier_boundary: MAX_U128, tier_size: 1,
    };
    // Create a custom asset configuration to test interesting risk factor scenarios.
    let synthetic_info = AssetInfoTrait::new(
        asset_name: 'BTC_1', :risk_factor_data, oracles_len: 1,
    );
    let asset_id = synthetic_info.asset_id;

    let mut state: FlowTestBase = FlowTestBaseTrait::new();

    state.facade.add_active_synthetic(synthetic_info: @synthetic_info, initial_price: 100);

    // Create users.
    let deleveraged_user = state.new_user_with_position();
    let deleverager_user = state.new_user_with_position();

    // Deposit to users.
    let deposit_info_user_1 = state
        .facade
        .deposit(
            depositor: deleverager_user.account,
            position_id: deleverager_user.position_id,
            quantized_amount: 100000,
        );
    state.facade.process_deposit(deposit_info: deposit_info_user_1);

    // Create orders.
    // User willing to buy 2 synthetic assets for 168 (quote) + 20 (fee).
    let order_deleveraged_user = state
        .facade
        .create_order(
            user: deleveraged_user,
            base_amount: 2,
            base_asset_id: asset_id,
            quote_amount: -168,
            fee_amount: 20,
        );

    let order_deleverager_user = state
        .facade
        .create_order(
            user: deleverager_user,
            base_amount: -2,
            base_asset_id: asset_id,
            quote_amount: 168,
            fee_amount: 20,
        );

    // Make trades.
    // User recieves 2 synthetic asset for 168 (quote) + 20 (fee).
    state
        .facade
        .trade(
            order_info_a: order_deleveraged_user,
            order_info_b: order_deleverager_user,
            base: 2,
            quote: -168,
            fee_a: 20,
            fee_b: 20,
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -188 + 2 * 100 = 12                 2 * 100 * 0.01 = 2           6
    state
        .facade
        .validate_total_value(position_id: deleveraged_user.position_id, expected_total_value: 12);
    state
        .facade
        .validate_total_risk(position_id: deleveraged_user.position_id, expected_total_risk: 2);

    advance_time(10000);
    let mut new_funding_index = FundingIndex { value: 7 * FUNDING_SCALE };
    state
        .facade
        .funding_tick(
            funding_ticks: array![
                FundingTick { asset_id: asset_id, funding_index: new_funding_index },
            ]
                .span(),
        );

    //                            TV                                  TR                 TV / TR
    //                COLLATERAL*1 + SYNTHETIC*PRICE        |SYNTHETIC*PRICE*RISK|
    // deleveraged User:   -202 + 2 * 100 = -2                 2 * 100 * 0.01 = 2          - 1
    state
        .facade
        .validate_total_value(position_id: deleveraged_user.position_id, expected_total_value: -2);
    state
        .facade
        .validate_total_risk(position_id: deleveraged_user.position_id, expected_total_risk: 2);

    assert(
        state.facade.is_deleveragable(position_id: deleveraged_user.position_id),
        'user is not deleveragable',
    );

    state
        .facade
        .deleverage(
            deleveraged_user: deleveraged_user,
            deleverager_user: deleverager_user,
            base_asset_id: asset_id,
            deleveraged_base: -2,
            deleveraged_quote: 204,
        );
}
