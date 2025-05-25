use core::gas::{GasReserve, gas_reserve_create, gas_reserve_utilize};

// Allow implicitly dropping GasReserve instances.
impl GasReserveDrop of Drop<GasReserve> {}

#[test]
fn test_create_and_utilize_gas_reserve() {
    let gas0 = crate::testing::get_available_gas();

    // Buy and redeposit a GasReserve.
    let reserve1 = gas_reserve_create(1000).unwrap();
    let gas1 = crate::testing::get_available_gas();
    gas_reserve_utilize(reserve1);
    let gas2 = crate::testing::get_available_gas();

    // Buy the rest of the available gas.
    let reserve2 = gas_reserve_create(gas2).unwrap();
    let gas3 = crate::testing::get_available_gas();

    // Try to buy another reserve. This should fail.
    let reserve3_opt = gas_reserve_create(1);

    gas_reserve_utilize(reserve2);
    assert!(reserve3_opt.is_none());

    assert_eq!(gas1, gas0 - 1000);
    assert_eq!(gas2, gas0);
    assert_eq!(gas3, 0);
}
