use starknet::testing::set_caller_address;
use crate::contracts::erc20::{IERC20DispatcherTrait, IERC20LibraryDispatcher, erc_20};

#[test]
fn test_erc20_transfer() {
    set_caller_address(2.try_into().unwrap());
    IERC20LibraryDispatcher { class_hash: erc_20::TEST_CLASS_HASH }
        .transfer(1.try_into().unwrap(), 0_u256);
}
