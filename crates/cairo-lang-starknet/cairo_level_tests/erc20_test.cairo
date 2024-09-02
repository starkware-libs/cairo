use cairo_level_tests::contracts::erc20::{erc_20, IERC20LibraryDispatcher, IERC20DispatcherTrait};
use starknet::testing::set_caller_address;

#[test]
fn test_erc20_transfer() {
    let class_hash = erc_20::TEST_CLASS_HASH.try_into().unwrap();

    set_caller_address(starknet::const_value::<2>());
    IERC20LibraryDispatcher { class_hash }.transfer(starknet::const_value::<1>(), 0_u256);
}
