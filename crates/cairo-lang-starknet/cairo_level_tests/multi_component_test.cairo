use crate::components::mintable::{MintTraitDispatcher, MintTraitDispatcherTrait};
use crate::contracts::multi_component::{
    GetSupplyDispatcher, GetSupplyDispatcherTrait, contract_with_4_components,
};
use super::utils::serialized;

#[test]
fn test_flow() {
    // Set up.
    let recipient = starknet::contract_address_const::<0x1337>();
    let (contract_address, _) = starknet::syscalls::deploy_syscall(
        contract_with_4_components::TEST_CLASS_HASH.try_into().unwrap(),
        0,
        serialized((('name', 'symbol'), (18_u8, 1000_u256, recipient), recipient)),
        false,
    )
        .unwrap();
    let mut get_supply_dispatcher = GetSupplyDispatcher { contract_address };
    assert_eq!(get_supply_dispatcher.get_total_supply_plus_1(), 1001);
    let mut mint_dispatcher = MintTraitDispatcher { contract_address };
    starknet::testing::set_contract_address(recipient);
    mint_dispatcher.mint(recipient, 1999_u256);
    assert_eq!(get_supply_dispatcher.get_total_supply_plus_1(), 3000);
}
