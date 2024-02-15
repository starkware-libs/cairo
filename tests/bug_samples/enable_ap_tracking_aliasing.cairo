use starknet::ContractAddress;
use starknet::contract_address_try_from_felt252;
use starknet::contract_address_to_felt252;
use core::array::array_new;
use core::integer::{u128s_from_felt252, U128sFromFelt252Result};

#[inline(always)]
fn u256_from_felt252_inlined(v: felt252) -> u256 {
    match u128s_from_felt252(v) {
        U128sFromFelt252Result::Narrow(low) => u256 { low, high: 0_u128 },
        U128sFromFelt252Result::Wide((high, low)) => u256 { low, high },
    }
}
#[test]
#[inline(never)]
fn test_contract_address_ordering() {
    let addr10000: ContractAddress = match contract_address_try_from_felt252(0x10000) {
        Option::Some(addr) => addr,
        Option::None => {
            core::internal::revoke_ap_tracking();
            panic(array_new())
        }
    };
    let addr20000: ContractAddress = match contract_address_try_from_felt252(0x20000) {
        Option::Some(addr) => addr,
        Option::None => {
            core::internal::revoke_ap_tracking();
            panic(array_new())
        }
    };
    let lhs: u256 = u256_from_felt252_inlined(contract_address_to_felt252(addr10000));
    let rhs: u256 = u256_from_felt252_inlined(contract_address_to_felt252(addr20000));
    if lhs < rhs {
    } else {
    }
}
