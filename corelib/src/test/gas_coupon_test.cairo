use core::gas::{GasCoupon, gas_coupon_buy, gas_coupon_redeposit};

// Allow implicitly dropping GasCoupon instances.
impl GasCouponDrop of Drop<GasCoupon> {}

#[test]
fn test_buy_and_gas_coupon_redeposit() {
    let gas0 = crate::testing::get_available_gas();

    // Buy and redeposit a GasCoupon.
    let coupon1 = gas_coupon_buy(1000).unwrap();
    let gas1 = crate::testing::get_available_gas();
    gas_coupon_redeposit(coupon1);
    let gas2 = crate::testing::get_available_gas();

    // Buy the rest of the available gas.
    let coupon2 = gas_coupon_buy(gas2).unwrap();
    let gas3 = crate::testing::get_available_gas();

    // Try to buy another coupon. This should fail.
    let coupon3_opt = gas_coupon_buy(1);

    gas_coupon_redeposit(coupon2);
    assert!(coupon3_opt.is_none());

    assert_eq!(gas1, gas0 - 1000);
    assert_eq!(gas2, gas0);
    assert_eq!(gas3, 0);
}
