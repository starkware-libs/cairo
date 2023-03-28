#[test]
#[should_panic(expected = ('assert(false)', ))]
fn test_assert_false() {
    assert(false, 'assert(false)');
}

#[test]
fn test_assert_true() {
    assert(true, 'assert(true)');
}

#[test]
fn test_get_available_gas_no_gas_supply() {
    assert(testing::get_available_gas() == 0_u128, 'expected no_gas_supply')
}

#[test]
#[available_gas(10000)]
fn test_get_available_gas_with_gas_supply() {
    assert(testing::get_available_gas() > 5000_u128, 'high amount of gas used')
}
