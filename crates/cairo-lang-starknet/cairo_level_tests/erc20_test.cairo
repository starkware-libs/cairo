use starknet::contract_address::contract_address_const;
use starknet::testing::set_caller_address;
use crate::contracts::erc20::{IERC20DispatcherTrait, IERC20LibraryDispatcher, erc_20};

#[test]
fn test_erc20_transfer() {
    let class_hash = erc_20::TEST_CLASS_HASH.try_into().unwrap();

    set_caller_address(contract_address_const::<2_felt252>());
    let contract_address = contract_address_const::<1_felt252>();
    IERC20LibraryDispatcher { class_hash }.transfer(contract_address, 0_u256);
}
