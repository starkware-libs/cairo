use crate::components::mintable::{MintTraitDispatcher, MintTraitDispatcherTrait};
use crate::contracts::multi_component::{
    GetSupplyDispatcher, GetSupplyDispatcherTrait, contract_with_4_components,
};
use super::utils::serialized;

#[test]
fn test_flow() {
    // Set up.
    const RECIPIENT: starknet::ContractAddress = 0x1337_felt252.try_into().unwrap();
    let (contract_address, _) = starknet::syscalls::deploy_syscall(
        contract_with_4_components::TEST_CLASS_HASH,
        0,
        serialized((('name', 'symbol'), (18_u8, 1000_u256, RECIPIENT), RECIPIENT)),
        false,
    )
        .unwrap();
    let mut get_supply_dispatcher = GetSupplyDispatcher { contract_address };
    assert_eq!(get_supply_dispatcher.get_total_supply_plus_1(), 1001);
    let mut mint_dispatcher = MintTraitDispatcher { contract_address };
    starknet::testing::set_contract_address(RECIPIENT);
    mint_dispatcher.mint(RECIPIENT, 1999_u256);
    assert_eq!(get_supply_dispatcher.get_total_supply_plus_1(), 3000);
}
