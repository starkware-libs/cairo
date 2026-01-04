use perpetuals::tests::flow_tests::infra::*;
use perpetuals::tests::flow_tests::perps_tests_facade::User;

#[test]
fn test_two_users_two_synthetics() {
    /// Link to spreadsheet with calculations:
    /// https://docs.google.com/spreadsheets/d/1BIJ6Oq7hAsF-Vb6EJSQFYbCQJMyrncuWDC4NoKLiV1U/edit?gid=0#gid=0

    let mut test = FlowTestExtendedTrait::new(fee_percentage: 1);
    let user_a = test.new_user();
    let user_b = test.new_user();

    test.process_deposit(test.deposit(user_a, 10_000));
    test.process_deposit(test.deposit(user_b, 10_000));

    let sell_eth_a = test.create_order_request(user: user_a, asset: ETH_ASSET, base: -10);
    let buy_eth_b = test.create_order_request(user: user_b, asset: ETH_ASSET, base: 200);
    let (buy_eth_b, _) = test.trade(buy_eth_b, sell_eth_a);

    let buy_btc_a = test.create_order_request(user: user_a, asset: BTC_ASSET, base: 1);
    let sell_btc_b = test.create_order_request(user: user_b, asset: BTC_ASSET, base: -2);
    let (_, sell_btc_b) = test.trade(buy_btc_a, sell_btc_b);
    let sell_eth_a = test.create_order_request(user: user_a, asset: ETH_ASSET, base: -10);

    ////                collateral (1st fee   2nd fee)   |    BTC    |    ETH
    ///                   14_035   (  -51       -10  )   |  1*1024   |  -10*512    = 9_939
    test.validate_total_value(user_a, 9_939);

    ////                 BTC       |     ETH
    ///               1*1024*0.1   |  10*512*0.1 = 614
    test.validate_total_risk(user_a, 614);

    ////                collateral  (1st fee   2nd fee)   |    BTC    |    ETH
    ///                   5_843    (  -51       -10  )    |  -1*1024   |  10*512   = 9_939
    test.validate_total_value(user_b, 9_939);

    ////                 BTC       |     ETH
    ///               1*1024*0.1   |  10*512*0.1 = 614
    test.validate_total_risk(user_b, 614);

    test.hourly_funding_tick(array![(BTC_ASSET, 1), (ETH_ASSET, -1)].span());

    ////                  provisional balance   |   BTC tick      |    ETH tick
    ///                          9_939          |  1 * (0 - 1)    | -10 * (0 - (-1))   = 9_928
    test.validate_total_value(user_a, 9_928);

    ////                  provisional balance   |   BTC tick      |    ETH tick
    ///                          9_939          | -1 * (0 - 1)    |  10 * (0 - (-1))   = 9_950
    test.validate_total_value(user_b, 9_950);

    test.price_tick(array![(BTC_ASSET, 1100), (ETH_ASSET, 600)].span());

    ////                collateral |    BTC    |    ETH
    ///                   14_024   |  1*1100   |  -10*600  = 9_124
    test.validate_total_value(user_a, 9124);

    ////                 BTC       |     ETH
    ///               1*1100*0.1   |  10*600*0.1 = 710
    test.validate_total_risk(user_a, 710);

    ////                collateral |    BTC    |    ETH
    ///                   5_854   |  -1*1100   |  10*600  = 10_754
    test.validate_total_value(user_b, 10_754);

    ////                 BTC       |     ETH
    ///               1*1100*0.1   |  10*600*0.1 = 710
    test.validate_total_risk(user_b, 710);

    let buy_btc_a = test.create_order_request(user: user_a, asset: BTC_ASSET, base: 1);
    test.trade(buy_btc_a, sell_btc_b);

    ////                collateral  fee
    ///                   9124     - 11     = 9113
    test.validate_total_value(user_a, 9113);

    ////                collateral  fee
    ///                   10_754     - 10     = 10_744
    test.validate_total_value(user_b, 10_744);

    test.trade(sell_eth_a, buy_eth_b);

    ////                 old TV     sold       fee     received
    ///                   9113    -10*600     - 51    + 10*512
    test.validate_total_value(user_a, 8182);

    ////                 BTC       |     ETH
    ///               2*1100*0.1   |  20*600*0.2 = 2620
    test.validate_total_risk(user_a, 2620);

    ////                 old TV     bought       fee     paid
    ///                  10_744      10*600     - 51    + 10*512
    test.validate_total_value(user_b, 11_573);

    ////                 BTC       |     ETH
    ///               2*1100*0.1   |  20*600*0.2 = 2620
    test.validate_total_risk(user_b, 2620);
}

#[test]
fn test_two_users_ten_synthetics_flow() {
    let mut test = FlowTestExtendedTrait::new(fee_percentage: 1);
    let user_a = test.new_user();
    let user_b = test.new_user();

    test.process_deposit(test.deposit(user_a, 2_000_000));
    test.process_deposit(test.deposit(user_b, 2_000_000));

    let sell_btc_a = test.create_order_request(user: user_a, asset: BTC_ASSET, base: -1);
    let buy_btc_b = test.create_order_request(user: user_b, asset: BTC_ASSET, base: 1);
    test.trade(buy_btc_b, sell_btc_a);

    let sell_eth_a = test.create_order_request(user: user_a, asset: ETH_ASSET, base: -2);
    let buy_eth_b = test.create_order_request(user: user_b, asset: ETH_ASSET, base: 2);
    test.trade(buy_eth_b, sell_eth_a);

    let sell_strk_a = test.create_order_request(user: user_a, asset: STRK_ASSET, base: -4);
    let buy_strk_b = test.create_order_request(user: user_b, asset: STRK_ASSET, base: 4);
    test.trade(buy_strk_b, sell_strk_a);

    let sell_sol_a = test.create_order_request(user: user_a, asset: SOL_ASSET, base: -8);
    let buy_sol_b = test.create_order_request(user: user_b, asset: SOL_ASSET, base: 8);
    test.trade(buy_sol_b, sell_sol_a);

    let sell_doge_a = test.create_order_request(user: user_a, asset: DOGE_ASSET, base: -16);
    let buy_doge_b = test.create_order_request(user: user_b, asset: DOGE_ASSET, base: 16);
    test.trade(buy_doge_b, sell_doge_a);

    let buy_pepe_a = test.create_order_request(user: user_a, asset: PEPE_ASSET, base: 32);
    let sell_pepe_b = test.create_order_request(user: user_b, asset: PEPE_ASSET, base: -32);
    test.trade(buy_pepe_a, sell_pepe_b);

    let buy_etc_a = test.create_order_request(user: user_a, asset: ETC_ASSET, base: 64);
    let sell_etc_b = test.create_order_request(user: user_b, asset: ETC_ASSET, base: -64);
    test.trade(buy_etc_a, sell_etc_b);

    let buy_tao_a = test.create_order_request(user: user_a, asset: TAO_ASSET, base: 128);
    let sell_tao_b = test.create_order_request(user: user_b, asset: TAO_ASSET, base: -128);
    test.trade(buy_tao_a, sell_tao_b);

    let buy_xrp_a = test.create_order_request(user: user_a, asset: XRP_ASSET, base: 256);
    let sell_xrp_b = test.create_order_request(user: user_b, asset: XRP_ASSET, base: -256);
    test.trade(buy_xrp_a, sell_xrp_b);

    let buy_ada_a = test.create_order_request(user: user_a, asset: ADA_ASSET, base: 512);
    let sell_ada_b = test.create_order_request(user: user_b, asset: ADA_ASSET, base: -512);
    test.trade(buy_ada_a, sell_ada_b);

    test.validate_total_value(user_a, 1_999_900); // 2_000_000 - 100 (fees)
    test.validate_total_risk(user_a, 1_020);

    test.validate_total_value(user_b, 1_999_900); // 2_000_000 - 100 (fees)
    test.validate_total_risk(user_b, 1_020);

    test
        .price_tick(
            array![
                (BTC_ASSET, 1_024 + 100), // Value diff user_a: -100, user_b: +100. Risk(both): 112
                (ETH_ASSET, 512 + 100), // Value diff user_a: -200, user_b: +200. Risk(both): 122
                (STRK_ASSET, 256 + 100), // Value diff user_a: -400, user_b: +400. Risk(both): 142
                (SOL_ASSET, 128 + 100), // Value diff user_a: -800, user_b: +800. Risk(both): 182
                (DOGE_ASSET, 64 + 100), // Value diff user_a: -1600, user_b: +1600. Risk(both): 262
                (PEPE_ASSET, 32 + 100), // Value diff user_a: +3200, user_b: -3200. Risk(both): 422
                (ETC_ASSET, 16 + 100), // Value diff user_a: +6400, user_b: -6400. Risk(both): 742
                (TAO_ASSET, 8 + 100), // Value diff user_a: +12800, user_b: -12800. Risk(both): 2764
                (XRP_ASSET, 4 + 100), // Value diff user_a: +25600, user_b: -25600 Risk(both): 13312
                (ADA_ASSET, 2 + 100) // Value diff user_a: +51200, user_b: -51200 Risk(both): 26112
            ]
                .span() // Total value diff: user_a: +96100, user_b: -96100. Total risk:(both) 44_172
        );

    test
        .hourly_funding_tick(
            array![
                (BTC_ASSET, 1), // Value diff user_a: +1, user_b: -1
                (ETH_ASSET, -1), // Value diff user_a: -2, user_b: +2
                (STRK_ASSET, 1), // Value diff user_a: +4, user_b: -4
                (SOL_ASSET, -1), // Value diff user_a: -8, user_b: +8
                (DOGE_ASSET, 1), // Value diff user_a: +16, user_b: -16
                (PEPE_ASSET, -1), // Value diff user_a: +32, user_b: -32
                (ETC_ASSET, 1), // Value diff user_a: -64, user_b: +64
                (TAO_ASSET, -1), // Value diff user_a: +128, user_b: -128
                (XRP_ASSET, 1), // Value diff user_a: -256, user_b: +256
                (ADA_ASSET, -1) // Value diff user_a: +512, user_b: -512
            ]
                .span() // Total value diff: user_a: +363, user_b: -363
        );

    // user_a total value: 2_000_000 - 100(fees) + 96100(price tick) + 363(funding_tick)
    test.validate_total_value(user_a, 2_096_363);
    test.validate_total_risk(user_a, 44_172);

    // user_b total value: 2_000_000 - 100(fees) - 96100(price tick) - 363(funding_tick)
    test.validate_total_value(user_b, 1_903_437);
    test.validate_total_risk(user_b, 44_172);

    test.withdraw(test.withdraw_request(user_a, 1_000_000));
    test.withdraw(test.withdraw_request(user_b, 1_000_000));

    test.transfer(test.transfer_request(sender: user_a, recipient: user_b, amount: 1_000_000));

    // user_a total value: 2_096_363 - 1_000_000(withdraw) - 1_000_000(transfer) = 96_363
    test.validate_total_value(user_a, 96_363);
    test.validate_total_risk(user_a, 44_172);

    // user_b total value: 1_903_437 - 1_000_000(withdraw) + 1_000_000(transfer) = 1_903_437
    test.validate_total_value(user_b, 1_903_437);
    test.validate_total_risk(user_b, 44_172);
}

#[test]
fn test_long_deleverage_after_funding_tick() {
    // Setup:
    let mut test = FlowTestExtendedTrait::new(fee_percentage: 1);
    let deleveraged_user = test.new_user();
    let deleverager_user = test.new_user();
    let other_user = test.new_user();

    test.process_deposit(test.deposit(deleveraged_user, 113));
    test.process_deposit(test.deposit(deleverager_user, 100_000));
    test.process_deposit(test.deposit(other_user, 100_000));

    let order_a = test.create_order_request(user: deleveraged_user, asset: ETH_ASSET, base: 2);
    let order_b = test.create_order_request(user: deleverager_user, asset: ETH_ASSET, base: -1);
    let order_c = test.create_order_request(user: other_user, asset: ETH_ASSET, base: -1);

    let (order_a, _) = test.trade(order_a, order_b);
    test.trade(order_a, order_c);

    //                                      TV                              TR               TV / TR
    //                     (COLLATERAL+SYNTHETIC*PRICE)          (|SYNTHETIC*PRICE*RISK|)
    //                   113 + 2 * -517(5 fee) + 2 * 512 = 103     2 * 512 * 0.1 = 102        1.0098
    test.validate_total_value(deleveraged_user, 103);
    test.validate_total_risk(deleveraged_user, 102);

    // Test:

    // Maximum funding tick in an hour is 15.
    test.hourly_funding_tick(array![(ETH_ASSET, 15)].span());
    test.hourly_funding_tick(array![(ETH_ASSET, 30)].span());
    test.hourly_funding_tick(array![(ETH_ASSET, 45)].span());
    test.hourly_funding_tick(array![(ETH_ASSET, 60)].span());
    test.hourly_funding_tick(array![(ETH_ASSET, 75)].span());
    test.hourly_funding_tick(array![(ETH_ASSET, 90)].span());
    test.hourly_funding_tick(array![(ETH_ASSET, 103)].span());

    //                                  TV                             TR                    TV / TR
    //                    (Previous TV - funding)           (|SYNTHETIC*PRICE*RISK|)
    //                      103 - 2 * 103 = -103              2 * 512 * 0.1 = 102            -1.0098
    test.validate_total_value(deleveraged_user, -103);
    test.validate_total_risk(deleveraged_user, 102);

    test.deleverage(:deleveraged_user, :deleverager_user, asset: ETH_ASSET, base: -1, quote: 564);

    //                                  TV                                       TR          TV / TR
    //                       (Previous TV + extra)           (|SYNTHETIC*PRICE*RISK|)
    //                          -103 + 52 = -51               1 * 512 * 0.1 = 51               -1
    test.validate_total_value(deleveraged_user, -51);
    test.validate_total_risk(deleveraged_user, 51);
}

#[test]
fn test_long_deleverage_after_price_tick() {
    // Setup:
    let mut test = FlowTestExtendedTrait::new(fee_percentage: 1);
    let deleveraged_user = test.new_user();
    let deleverager_user = test.new_user();

    test.process_deposit(test.deposit(deleveraged_user, 113));
    test.process_deposit(test.deposit(deleverager_user, 100_000));

    let order_a = test.create_order_request(user: deleveraged_user, asset: ETH_ASSET, base: 2);
    let order_b = test.create_order_request(user: deleverager_user, asset: ETH_ASSET, base: -2);

    test.trade(order_a, order_b);

    //                                      TV                              TR               TV / TR
    //                     (COLLATERAL+SYNTHETIC*PRICE)           (|SYNTHETIC*PRICE*RISK|)
    //                   113 + 2 * -517(5 fee) + 2 * 512 = 103     2 * 512 * 0.1 = 102        1.0098
    test.validate_total_value(deleveraged_user, 103);
    test.validate_total_risk(deleveraged_user, 102);

    // Test:

    test.price_tick(array![(ETH_ASSET, 512 - 93)].span());

    //                                      TV                               TR              TV / TR
    //                            (Previous TV - price diff)       (|SYNTHETIC*PRICE*RISK|)
    //                               103  - 2 * 93  = -83             1 * 419 * 0.1 = 41        1
    test.validate_total_value(deleveraged_user, -83);
    test.validate_total_risk(deleveraged_user, 83);

    test.deleverage(:deleveraged_user, :deleverager_user, asset: ETH_ASSET, base: -1, quote: 461);

    //                                      TV                           TR                  TV / TR
    //                            (Previous TV + extra)        (|SYNTHETIC*PRICE*RISK|)
    //                               -83  + 42 = -41              1 * 419 * 0.1 = 41            1
    test.validate_total_value(deleveraged_user, -41);
    test.validate_total_risk(deleveraged_user, 41);
}

#[test]
fn test_short_deleverage_after_funding_tick() {
    // Setup:
    let mut test = FlowTestExtendedTrait::new(fee_percentage: 1);
    let deleveraged_user = test.new_user();
    let deleverager_user = test.new_user();
    let other_user = test.new_user();

    test.process_deposit(test.deposit(deleveraged_user, 113));
    test.process_deposit(test.deposit(deleverager_user, 100_000));
    test.process_deposit(test.deposit(other_user, 100_000));

    let order_a = test.create_order_request(user: deleveraged_user, asset: ETH_ASSET, base: -2);
    let order_b = test.create_order_request(user: deleverager_user, asset: ETH_ASSET, base: 1);
    let order_c = test.create_order_request(user: other_user, asset: ETH_ASSET, base: 1);

    let (_, order_a) = test.trade(order_a, order_b);
    test.trade(order_a, order_c);

    //                                      TV                              TR               TV / TR
    //                     (COLLATERAL+SYNTHETIC*PRICE)          (|SYNTHETIC*PRICE*RISK|)
    //                   113 + 2 * +507(5 fee) - 2 * 512 = 103     2 * 512 * 0.1 = 102        1.0098
    test.validate_total_value(deleveraged_user, 103);
    test.validate_total_risk(deleveraged_user, 102);

    // Test:

    // Maximum funding tick in an hour is 15.
    test.hourly_funding_tick(array![(ETH_ASSET, -15)].span());
    test.hourly_funding_tick(array![(ETH_ASSET, -30)].span());
    test.hourly_funding_tick(array![(ETH_ASSET, -45)].span());
    test.hourly_funding_tick(array![(ETH_ASSET, -60)].span());
    test.hourly_funding_tick(array![(ETH_ASSET, -75)].span());
    test.hourly_funding_tick(array![(ETH_ASSET, -90)].span());
    test.hourly_funding_tick(array![(ETH_ASSET, -103)].span());

    //                                  TV                                       TR          TV / TR
    //                         Previous TV - funding)         (|SYNTHETIC*PRICE*RISK|)
    //                          103 - 2*103 = -103                1 * 512 * 0.1 = 51         -1.0098
    test.validate_total_value(deleveraged_user, -103);
    test.validate_total_risk(deleveraged_user, 102);

    test.deleverage(:deleveraged_user, :deleverager_user, asset: ETH_ASSET, base: 1, quote: -460);

    //                                  TV                                       TR          TV / TR
    //                       (Previous TV + extra)          (|SYNTHETIC*PRICE*RISK|)
    //                          -103 + 52 = -51               1 * 512 * 0.1 = 51               -1
    test.validate_total_value(deleveraged_user, -51);
    test.validate_total_risk(deleveraged_user, 51);
}

#[test]
fn test_short_deleverage_after_price_tick() {
    // Setup:
    let mut test = FlowTestExtendedTrait::new(fee_percentage: 1);
    let deleveraged_user = test.new_user();
    let deleverager_user = test.new_user();

    test.process_deposit(test.deposit(deleveraged_user, 113));
    test.process_deposit(test.deposit(deleverager_user, 100_000));

    let order_a = test.create_order_request(user: deleveraged_user, asset: ETH_ASSET, base: -2);
    let order_b = test.create_order_request(user: deleverager_user, asset: ETH_ASSET, base: 2);

    test.trade(order_a, order_b);

    //                                      TV                              TR               TV / TR
    //                     (COLLATERAL+SYNTHETIC*PRICE)           (|SYNTHETIC*PRICE*RISK|)
    //                   113 + 2 * -517(5 fee) + 2 * 512 = 103     2 * 512 * 0.1 = 102        1.0098
    test.validate_total_value(deleveraged_user, 103);
    test.validate_total_risk(deleveraged_user, 102);

    // Test:

    test.price_tick(array![(ETH_ASSET, 512 + 114)].span());

    //                                  TV                               TR                  TV / TR
    //                    (Previous TV - price diff)       (|SYNTHETIC*PRICE*RISK|)
    //                      103  - 2 * 114  = -125       2 * (512 + 114) * 0.1 =  125          -1
    test.validate_total_value(deleveraged_user, -125);
    test.validate_total_risk(deleveraged_user, 125);

    test.deleverage(:deleveraged_user, :deleverager_user, asset: ETH_ASSET, base: 1, quote: -563);

    //                                      TV                           TR                  TV / TR
    //                            (Previous TV + extra)        (|SYNTHETIC*PRICE*RISK|)
    //                               -125  + 63 = -62             1 * 626 * 0.1 = 62            1
    test.validate_total_value(deleveraged_user, -62);
    test.validate_total_risk(deleveraged_user, 62);
}

#[test]
fn test_long_liquidate_after_funding_tick() {
    // Setup:
    let mut test = FlowTestExtendedTrait::new(fee_percentage: 1);
    let liquidated_user = test.new_user();
    let liquidator_user = test.new_user();
    let other_user = test.new_user();

    test.process_deposit(test.deposit(liquidated_user, 113));
    test.process_deposit(test.deposit(liquidator_user, 100_000));
    test.process_deposit(test.deposit(other_user, 100_000));

    let order_a = test.create_order_request(user: liquidated_user, asset: ETH_ASSET, base: 2);
    let order_b = test.create_order_request(user: other_user, asset: ETH_ASSET, base: -2);

    test.trade(order_a, order_b);

    //                                      TV                              TR               TV / TR
    //                     (COLLATERAL+SYNTHETIC*PRICE)          (|SYNTHETIC*PRICE*RISK|)
    //                   113 + 2 * -517(5 fee) + 2 * 512 = 103     2 * 512 * 0.1 = 102        1.0098
    test.validate_total_value(liquidated_user, 103);
    test.validate_total_risk(liquidated_user, 102);

    // Test:

    // Maximum funding tick in an hour is 15.
    test.hourly_funding_tick(array![(ETH_ASSET, 15)].span());
    test.hourly_funding_tick(array![(ETH_ASSET, 26)].span());

    //                                  TV                             TR                    TV / TR
    //                    (Previous TV - funding)           (|SYNTHETIC*PRICE*RISK|)
    //                      103 - 2 * 26 = 51                 2 * 512 * 0.1 = 102              0.5
    test.validate_total_value(liquidated_user, 51);
    test.validate_total_risk(liquidated_user, 102);

    // Liquidator  wants to buy 21 lower than current price. By lowering the price, creating an
    // order then bringing the price back, we get an order with a lower price.
    test.price_tick(array![(ETH_ASSET, 512 - 21)].span());
    let order_c = test.create_order_request(user: liquidator_user, asset: ETH_ASSET, base: 1);
    test.price_tick(array![(ETH_ASSET, 512)].span());

    test.liquidate(:liquidated_user, liquidator_order: order_c);

    //                                  TV                                TR                 TV / TR
    //                       (Previous TV - loss - fee)       |SYNTHETIC*PRICE*RISK|)
    //                            51 - 21 - 4 = 26               1 * 512 * 0.1 = 51           0.51
    test.validate_total_value(liquidated_user, 26);
    test.validate_total_risk(liquidated_user, 51);
}

#[test]
fn test_long_liquidate_after_price_tick() {
    // Setup:
    let mut test = FlowTestExtendedTrait::new(fee_percentage: 1);
    let liquidated_user = test.new_user();
    let liquidator_user = test.new_user();
    let other_user = test.new_user();

    test.process_deposit(test.deposit(liquidated_user, 113));
    test.process_deposit(test.deposit(liquidator_user, 100_000));
    test.process_deposit(test.deposit(other_user, 100_000));

    let order_a = test.create_order_request(user: liquidated_user, asset: ETH_ASSET, base: 2);
    let order_b = test.create_order_request(user: other_user, asset: ETH_ASSET, base: -2);

    test.trade(order_a, order_b);

    //                                      TV                              TR               TV / TR
    //                     (COLLATERAL+SYNTHETIC*PRICE)          (|SYNTHETIC*PRICE*RISK|)
    //                   113 + 2 * -517(5 fee) + 2 * 512 = 103     2 * 512 * 0.1 = 102        1.0098
    test.validate_total_value(liquidated_user, 103);
    test.validate_total_risk(liquidated_user, 102);

    // Test:

    test.price_tick(array![(ETH_ASSET, 512 - 27)].span());

    //                                  TV                             TR                    TV / TR
    //                     (Previous TV - price diff)       (|SYNTHETIC*PRICE*RISK|)
    //                        103 - 2 * 27 = 49               2 * 485 * 0.1 = 97              0.505
    test.validate_total_value(liquidated_user, 49);
    test.validate_total_risk(liquidated_user, 97);

    // Liquidator  wants to buy 19 lower than current price. By lowering the price, creating an
    // order then bringing the price back, we get an order with a lower price.
    test.price_tick(array![(ETH_ASSET, 512 - 27 - 19)].span());
    let order_c = test.create_order_request(user: liquidator_user, asset: ETH_ASSET, base: 1);
    test.price_tick(array![(ETH_ASSET, 512 - 27)].span());

    test.liquidate(:liquidated_user, liquidator_order: order_c);

    //                                  TV                                TR                 TV / TR
    //                       (Previous TV - loss - fee)       |SYNTHETIC*PRICE*RISK|)
    //                            49 - 19 - 4 = 26               1 * 485 * 0.1 = 48           0.541
    test.validate_total_value(liquidated_user, 26);
    test.validate_total_risk(liquidated_user, 48);
}

#[test]
fn test_short_liquidate_after_funding_tick() {
    // Setup:
    let mut test = FlowTestExtendedTrait::new(fee_percentage: 1);
    let liquidated_user = test.new_user();
    let liquidator_user = test.new_user();
    let other_user = test.new_user();

    test.process_deposit(test.deposit(liquidated_user, 113));
    test.process_deposit(test.deposit(liquidator_user, 100_000));
    test.process_deposit(test.deposit(other_user, 100_000));

    let order_a = test.create_order_request(user: liquidated_user, asset: ETH_ASSET, base: -2);
    let order_b = test.create_order_request(user: other_user, asset: ETH_ASSET, base: 2);

    test.trade(order_a, order_b);

    //                                      TV                              TR               TV / TR
    //                     (COLLATERAL+SYNTHETIC*PRICE)          (|SYNTHETIC*PRICE*RISK|)
    //                   113 + 2 * -517(5 fee) + 2 * 512 = 103     2 * 512 * 0.1 = 102        1.0098
    test.validate_total_value(liquidated_user, 103);
    test.validate_total_risk(liquidated_user, 102);

    // Test:

    // Maximum funding tick in an hour is 15.
    test.hourly_funding_tick(array![(ETH_ASSET, -15)].span());
    test.hourly_funding_tick(array![(ETH_ASSET, -26)].span());

    //                                  TV                             TR                    TV / TR
    //                    (Previous TV - funding)           (|SYNTHETIC*PRICE*RISK|)
    //                      103 - 2 * 26 = 51                  2 * 512 * 0.1 = 102              0.5
    test.validate_total_value(liquidated_user, 51);
    test.validate_total_risk(liquidated_user, 102);

    // Liquidator  wants to sell 20 higher than current price. By rising the price, creating an
    // order then lowering the price back, we get an order with a higher price.
    test.price_tick(array![(ETH_ASSET, 512 + 20)].span());
    let order_c = test.create_order_request(user: liquidator_user, asset: ETH_ASSET, base: -1);
    test.price_tick(array![(ETH_ASSET, 512)].span());

    test.liquidate(:liquidated_user, liquidator_order: order_c);

    //                                  TV                                TR                 TV / TR
    //                       (Previous TV - loss - fee)       |SYNTHETIC*PRICE*RISK|)
    //                            51 - 20 - 5 = 26               1 * 512 * 0.1 = 51           0.51
    test.validate_total_value(liquidated_user, 26);
    test.validate_total_risk(liquidated_user, 51);
}


#[test]
fn test_short_liquidate_after_price_tick() {
    // Setup:
    let mut test = FlowTestExtendedTrait::new(fee_percentage: 1);
    let liquidated_user = test.new_user();
    let liquidator_user = test.new_user();
    let other_user = test.new_user();

    test.process_deposit(test.deposit(liquidated_user, 113));
    test.process_deposit(test.deposit(liquidator_user, 100_000));
    test.process_deposit(test.deposit(other_user, 100_000));

    let order_a = test.create_order_request(user: liquidated_user, asset: ETH_ASSET, base: -2);
    let order_b = test.create_order_request(user: other_user, asset: ETH_ASSET, base: 2);

    test.trade(order_a, order_b);

    //                                      TV                              TR               TV / TR
    //                     (COLLATERAL+SYNTHETIC*PRICE)          (|SYNTHETIC*PRICE*RISK|)
    //                   113 + 2 * -517(5 fee) + 2 * 512 = 103     2 * 512 * 0.1 = 102        1.0098
    test.validate_total_value(liquidated_user, 103);
    test.validate_total_risk(liquidated_user, 102);

    // Test:

    test.price_tick(array![(ETH_ASSET, 512 + 27)].span());

    //                                  TV                             TR                    TV / TR
    //                     (Previous TV - price diff)       (|SYNTHETIC*PRICE*RISK|)
    //                        103 - 2 * 27 = 49               2 * 539 * 0.1 = 107              0.45
    test.validate_total_value(liquidated_user, 49);
    test.validate_total_risk(liquidated_user, 107);

    // Liquidator  wants to sell 18 higher than current price. By rising the price, creating an
    // order then lowering the price back, we get an order with a higher price.
    test.price_tick(array![(ETH_ASSET, 512 + 27 + 18)].span());
    let order_c = test.create_order_request(user: liquidator_user, asset: ETH_ASSET, base: -1);
    test.price_tick(array![(ETH_ASSET, 512 + 27)].span());

    test.liquidate(:liquidated_user, liquidator_order: order_c);

    //                                  TV                                TR                 TV / TR
    //                       (Previous TV - loss - fee)       |SYNTHETIC*PRICE*RISK|)
    //                            49 - 18 - 5 = 26               1 * 539 * 0.1 = 53           0.541
    test.validate_total_value(liquidated_user, 26);
    test.validate_total_risk(liquidated_user, 53);
}


#[test]
fn test_multi_trade_basic() {
    // Setup:
    let mut test = FlowTestExtendedTrait::new(fee_percentage: 1);
    let mut users: Array<User> = ArrayTrait::new();
    let mut orders: Array<OrderRequest> = ArrayTrait::new();
    let mut base = 1;
    for _ in 0_u8..10_u8 {
        let user = test.new_user();
        test.process_deposit(test.deposit(user, 10_000));
        users.append(user);
        orders.append(test.create_order_request(user: user, asset: ETH_ASSET, base: base));
        // alternate between long and short
        base = -base;
    }

    let mut trades: Array<(OrderRequest, OrderRequest)> = ArrayTrait::new();
    for i in 0_u32..5_u32 {
        let order_a = *orders[i * 2];
        let order_b = *orders[i * 2 + 1];
        trades.append((order_a, order_b));
    }
    let after_trades = test.multi_trade(settlements: trades.span());

    // order are fulfilled entirely
    for trade in after_trades {
        let (order_a, order_b) = *trade;
        assert_eq!(order_a.actual_base, 0_u64);
        assert_eq!(order_b.actual_base, 0_u64);
    }

    for user in users {
        // tv = balance - fee = 10_000 - 512 / 100 = 9995
        test.validate_total_value(user, 9995);

        // tr = 1 * 512 * 0.1 = 51
        test.validate_total_risk(user, 51);
    }
}

#[test]
fn test_multi_trade_empty() {
    let mut test = FlowTestExtendedTrait::new(fee_percentage: 1);
    let mut trades: Array<(OrderRequest, OrderRequest)> = ArrayTrait::new();
    let after_trades = test.multi_trade(settlements: trades.span());
    assert_eq!(after_trades.len(), 0);
}

#[test]
fn test_multi_trade_with_one_trade() {
    // Setup:
    let mut test = FlowTestExtendedTrait::new(fee_percentage: 1);
    let user_a = test.new_user();
    let user_b = test.new_user();

    test.process_deposit(test.deposit(user_a, 10_000));
    test.process_deposit(test.deposit(user_b, 10_000));

    let order_a = test.create_order_request(user: user_a, asset: ETH_ASSET, base: -1);
    let order_b = test.create_order_request(user: user_b, asset: ETH_ASSET, base: 1);
    let trades = array![(order_a, order_b)].span();

    // Test:
    let after_trades = test.multi_trade(settlements: trades);

    // Validate:
    // Orders are fulfilled entirely
    for trade in after_trades {
        let (order_a, order_b) = *trade;
        assert_eq!(order_a.actual_base, 0_u64);
        assert_eq!(order_b.actual_base, 0_u64);
    }

    test.validate_total_value(user_a, 9995);
    test.validate_total_risk(user_a, 51);
    test.validate_total_value(user_b, 9995);
    test.validate_total_risk(user_b, 51);
}

#[test]
fn test_multi_trade_single_market_maker_many_users() {
    // Setup:
    let mut test = FlowTestExtendedTrait::new(fee_percentage: 5);
    let mut users: Array<User> = ArrayTrait::new();
    let mut trades: Array<(OrderRequest, OrderRequest)> = ArrayTrait::new();

    let asset_names = array![
        BTC_ASSET, ETH_ASSET, STRK_ASSET, SOL_ASSET, DOGE_ASSET, PEPE_ASSET, ETC_ASSET, TAO_ASSET,
        XRP_ASSET, ADA_ASSET,
    ];

    // TV and TR for regular users
    let tvtrs: Array<(i128, u128)> = array![
        (19488, 2048), (19744, 512), (19872, 256), (19936, 128), (19968, 64), (19984, 32),
        (19992, 16), (19996, 8), (19998, 4), (19999, 2),
    ];

    // TV and TR for market maker
    let mm_tv = 998977;
    let mm_tr = 3070;

    // Init users
    const MM_INITIAL_BALANCE: u64 = 1_000_000;
    const USER_INITIAL_BALANCE: u64 = 20_000;
    let market_maker = test.new_user();
    test.process_deposit(test.deposit(market_maker, MM_INITIAL_BALANCE));
    for asset_name in asset_names {
        let user = test.new_user();
        test.process_deposit(test.deposit(user, USER_INITIAL_BALANCE));
        users.append(user);

        let order_a = test.create_order_request(user: market_maker, asset: asset_name, base: -10);
        let order_b = test.create_order_request(user: user, asset: asset_name, base: 10);
        trades.append((order_a, order_b));
    }

    // Test:
    let after_trades = test.multi_trade(settlements: trades.span());

    // Validate:
    // Orders are fulfilled entirely
    for trade in after_trades {
        let (order_a, order_b) = *trade;
        assert_eq!(order_a.actual_base, 0_u64);
        assert_eq!(order_b.actual_base, 0_u64);
    }

    for i in 0_u32..10_u32 {
        let user = *users[i];
        let (tv, tr): (i128, u128) = *tvtrs[i];
        test.validate_total_value(user, tv);
        test.validate_total_risk(user, tr);
    }

    test.validate_total_value(market_maker, mm_tv);
    test.validate_total_risk(market_maker, mm_tr);
}

#[test]
fn test_multi_trade_single_order() {
    // Setup:
    let mut test = FlowTestExtendedTrait::new(fee_percentage: 5);
    let mut users: Array<User> = ArrayTrait::new();
    let mut trades: Array<(OrderRequest, OrderRequest)> = ArrayTrait::new();

    // tv = balance - fee = balance - 5% of tx value = 20_000 - price / 20
    // tr = asset_balances * synthetic_price * risk = 1 * price * 0.1 = price / 10
    let user_tv = 19949;
    let user_tr = 102;
    //tv = balance - total_fee
    //tr = 10 * price * 0.2 = price * 2
    let mm_tv = 999490;
    let mm_tr = 2048;
    const MM_INITIAL_BALANCE: u64 = 1_000_000;
    const USER_INITIAL_BALANCE: u64 = 20_000;
    let market_maker = test.new_user();
    test.process_deposit(test.deposit(market_maker, MM_INITIAL_BALANCE));
    let mut main_order = test.create_order_request(user: market_maker, asset: BTC_ASSET, base: -10);
    for _ in 0_u8..10_u8 {
        let user = test.new_user();
        test.process_deposit(test.deposit(user, USER_INITIAL_BALANCE));
        users.append(user);

        let order = test.create_order_request(user: user, asset: BTC_ASSET, base: 1);
        trades.append((main_order, order));
        main_order
            .actual_base = if main_order.actual_base > 0 {
                main_order.actual_base - 1
            } else {
                0
            };
    }

    // Test:
    let after_trades = test.multi_trade(settlements: trades.span());

    // Validate:
    let mut i = 9;
    for trade in after_trades {
        let (main_order, order) = *trade;
        assert_eq!(main_order.actual_base, i);
        assert_eq!(order.actual_base, 0_u64);
        i = if i > 0 {
            i - 1
        } else {
            0
        };
    }

    for i in 0_u32..10_u32 {
        let user = *users[i];
        test.validate_total_value(user, user_tv);
        test.validate_total_risk(user, user_tr);
    }

    test.validate_total_value(market_maker, mm_tv);
    test.validate_total_risk(market_maker, mm_tr);
}

#[test]
fn test_multi_trade_vs_trade() {
    const INITIAL_BALANCE: u64 = 1_000_000;

    // Multi trade setup:
    let mut multi_trade_test = FlowTestExtendedTrait::new(fee_percentage: 5);
    let multi_trade_user_1 = multi_trade_test.new_user();
    let multi_trade_user_2 = multi_trade_test.new_user();
    let mut multi_trade_trades: Array<(OrderRequest, OrderRequest)> = ArrayTrait::new();
    multi_trade_test.process_deposit(multi_trade_test.deposit(multi_trade_user_1, INITIAL_BALANCE));
    multi_trade_test.process_deposit(multi_trade_test.deposit(multi_trade_user_2, INITIAL_BALANCE));

    // Trade setup:
    let mut trade_test = FlowTestExtendedTrait::new(fee_percentage: 5);
    let trade_user_1 = trade_test.new_user();
    let trade_user_2 = trade_test.new_user();
    trade_test.process_deposit(trade_test.deposit(trade_user_1, INITIAL_BALANCE));
    trade_test.process_deposit(trade_test.deposit(trade_user_2, INITIAL_BALANCE));

    for asset_name in array![
        BTC_ASSET, ETH_ASSET, STRK_ASSET, SOL_ASSET, DOGE_ASSET, PEPE_ASSET, ETC_ASSET, TAO_ASSET,
        XRP_ASSET, ADA_ASSET,
    ] {
        // Multi trade:
        let multi_trade_order_1 = multi_trade_test
            .create_order_request(user: multi_trade_user_1, asset: asset_name, base: -10);
        let multi_trade_order_2 = multi_trade_test
            .create_order_request(user: multi_trade_user_2, asset: asset_name, base: 10);
        multi_trade_trades.append((multi_trade_order_1, multi_trade_order_2));

        // Trade:
        let trade_order_1 = trade_test
            .create_order_request(user: trade_user_1, asset: asset_name, base: -10);
        let trade_order_2 = trade_test
            .create_order_request(user: trade_user_2, asset: asset_name, base: 10);
        trade_test.trade(trade_order_1, trade_order_2);
    }

    multi_trade_test.multi_trade(settlements: multi_trade_trades.span());

    // Cross validate:
    let multi_trade_position_data_1 = multi_trade_test.get_position_data(multi_trade_user_1);
    let multi_trade_position_data_2 = multi_trade_test.get_position_data(multi_trade_user_2);
    let trade_position_data_1 = trade_test.get_position_data(trade_user_1);
    let trade_position_data_2 = trade_test.get_position_data(trade_user_2);
    assert_eq!(multi_trade_position_data_1, trade_position_data_1);
    assert_eq!(multi_trade_position_data_2, trade_position_data_2);
}

#[test]
fn test_multi_trade_with_price_and_funding_ticks() {
    let mut test = FlowTestExtendedTrait::new(fee_percentage: 1);
    let mut trades: Array<(OrderRequest, OrderRequest)> = ArrayTrait::new();
    let user_a = test.new_user();
    let user_b = test.new_user();

    test.process_deposit(test.deposit(user_a, 2_000_000));
    test.process_deposit(test.deposit(user_b, 2_000_000));

    let sell_btc_a = test.create_order_request(user: user_a, asset: BTC_ASSET, base: -1);
    let buy_btc_b = test.create_order_request(user: user_b, asset: BTC_ASSET, base: 1);
    trades.append((buy_btc_b, sell_btc_a));

    let sell_eth_a = test.create_order_request(user: user_a, asset: ETH_ASSET, base: -2);
    let buy_eth_b = test.create_order_request(user: user_b, asset: ETH_ASSET, base: 2);
    trades.append((buy_eth_b, sell_eth_a));

    let sell_strk_a = test.create_order_request(user: user_a, asset: STRK_ASSET, base: -4);
    let buy_strk_b = test.create_order_request(user: user_b, asset: STRK_ASSET, base: 4);
    trades.append((buy_strk_b, sell_strk_a));

    let sell_sol_a = test.create_order_request(user: user_a, asset: SOL_ASSET, base: -8);
    let buy_sol_b = test.create_order_request(user: user_b, asset: SOL_ASSET, base: 8);
    trades.append((buy_sol_b, sell_sol_a));

    let sell_doge_a = test.create_order_request(user: user_a, asset: DOGE_ASSET, base: -16);
    let buy_doge_b = test.create_order_request(user: user_b, asset: DOGE_ASSET, base: 16);
    trades.append((buy_doge_b, sell_doge_a));

    let buy_pepe_a = test.create_order_request(user: user_a, asset: PEPE_ASSET, base: 32);
    let sell_pepe_b = test.create_order_request(user: user_b, asset: PEPE_ASSET, base: -32);
    trades.append((buy_pepe_a, sell_pepe_b));

    let buy_etc_a = test.create_order_request(user: user_a, asset: ETC_ASSET, base: 64);
    let sell_etc_b = test.create_order_request(user: user_b, asset: ETC_ASSET, base: -64);
    trades.append((buy_etc_a, sell_etc_b));

    let buy_tao_a = test.create_order_request(user: user_a, asset: TAO_ASSET, base: 128);
    let sell_tao_b = test.create_order_request(user: user_b, asset: TAO_ASSET, base: -128);
    trades.append((buy_tao_a, sell_tao_b));

    let buy_xrp_a = test.create_order_request(user: user_a, asset: XRP_ASSET, base: 256);
    let sell_xrp_b = test.create_order_request(user: user_b, asset: XRP_ASSET, base: -256);
    trades.append((buy_xrp_a, sell_xrp_b));

    let buy_ada_a = test.create_order_request(user: user_a, asset: ADA_ASSET, base: 512);
    let sell_ada_b = test.create_order_request(user: user_b, asset: ADA_ASSET, base: -512);
    trades.append((buy_ada_a, sell_ada_b));

    test.multi_trade(settlements: trades.span());

    test.validate_total_value(user_a, 1_999_900); // 2_000_000 - 100 (fees)
    test.validate_total_risk(user_a, 1_020);

    test.validate_total_value(user_b, 1_999_900); // 2_000_000 - 100 (fees)
    test.validate_total_risk(user_b, 1_020);

    test
        .price_tick(
            array![
                (BTC_ASSET, 1_024 + 100), // Value diff user_a: -100, user_b: +100. Risk(both): 112
                (ETH_ASSET, 512 + 100), // Value diff user_a: -200, user_b: +200. Risk(both): 122
                (STRK_ASSET, 256 + 100), // Value diff user_a: -400, user_b: +400. Risk(both): 142
                (SOL_ASSET, 128 + 100), // Value diff user_a: -800, user_b: +800. Risk(both): 182
                (DOGE_ASSET, 64 + 100), // Value diff user_a: -1600, user_b: +1600. Risk(both): 262
                (PEPE_ASSET, 32 + 100), // Value diff user_a: +3200, user_b: -3200. Risk(both): 422
                (ETC_ASSET, 16 + 100), // Value diff user_a: +6400, user_b: -6400. Risk(both): 742
                (TAO_ASSET, 8 + 100), // Value diff user_a: +12800, user_b: -12800. Risk(both): 2764
                (XRP_ASSET, 4 + 100), // Value diff user_a: +25600, user_b: -25600 Risk(both): 13312
                (ADA_ASSET, 2 + 100) // Value diff user_a: +51200, user_b: -51200 Risk(both): 26112
            ]
                .span() // Total value diff: user_a: +96100, user_b: -96100. Total risk:(both) 44_172
        );

    test
        .hourly_funding_tick(
            array![
                (BTC_ASSET, 1), // Value diff user_a: +1, user_b: -1
                (ETH_ASSET, -1), // Value diff user_a: -2, user_b: +2
                (STRK_ASSET, 1), // Value diff user_a: +4, user_b: -4
                (SOL_ASSET, -1), // Value diff user_a: -8, user_b: +8
                (DOGE_ASSET, 1), // Value diff user_a: +16, user_b: -16
                (PEPE_ASSET, -1), // Value diff user_a: +32, user_b: -32
                (ETC_ASSET, 1), // Value diff user_a: -64, user_b: +64
                (TAO_ASSET, -1), // Value diff user_a: +128, user_b: -128
                (XRP_ASSET, 1), // Value diff user_a: -256, user_b: +256
                (ADA_ASSET, -1) // Value diff user_a: +512, user_b: -512
            ]
                .span() // Total value diff: user_a: +363, user_b: -363
        );

    // user_a total value: 2_000_000 - 100(fees) + 96100(price tick) + 363(funding_tick)
    test.validate_total_value(user_a, 2_096_363);
    test.validate_total_risk(user_a, 44_172);

    // user_b total value: 2_000_000 - 100(fees) - 96100(price tick) - 363(funding_tick)
    test.validate_total_value(user_b, 1_903_437);
    test.validate_total_risk(user_b, 44_172);

    test.withdraw(test.withdraw_request(user: user_a, amount: 1_000_000));
    test.withdraw(test.withdraw_request(user: user_b, amount: 1_000_000));

    test.transfer(test.transfer_request(sender: user_a, recipient: user_b, amount: 1_000_000));

    // user_a total value: 2_096_363 - 1_000_000(withdraw) - 1_000_000(transfer) = 96_363
    test.validate_total_value(user_a, 96_363);
    test.validate_total_risk(user_a, 44_172);

    // user_b total value: 1_903_437 - 1_000_000(withdraw) + 1_000_000(transfer) = 1_903_437
    test.validate_total_value(user_b, 1_903_437);
    test.validate_total_risk(user_b, 44_172);
}
