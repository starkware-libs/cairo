use ERC20Component::InternalTrait;
use core::num::traits::Bounded;
use openzeppelin_test_common::math::{is_overflow_add, is_overflow_sub};
use openzeppelin_test_common::mocks::erc20::DualCaseERC20Mock;
use openzeppelin_testing::constants::{NAME, OWNER, RECIPIENT, SPENDER, SYMBOL};
use snforge_std::{start_cheat_caller_address, test_address};
use starknet::ContractAddress;
use crate::erc20::ERC20Component;
use crate::erc20::ERC20Component::{ERC20CamelOnlyImpl, ERC20Impl, ERC20MetadataImpl, InternalImpl};

//
// Setup
//

type ComponentState = ERC20Component::ComponentState<DualCaseERC20Mock::ContractState>;

fn COMPONENT_STATE() -> ComponentState {
    ERC20Component::component_state_for_testing()
}

fn setup(supply: u256) -> ComponentState {
    let mut state = COMPONENT_STATE();
    state.initializer(NAME(), SYMBOL());
    state.mint(OWNER, supply);
    state
}

//
// Tests
//

#[test]
#[fuzzer]
fn test_mint(supply: u256, mint_amount: u256) {
    if is_overflow_add(supply, mint_amount) {
        return;
    }
    let mut state = setup(supply);

    assert_total_supply(supply);
    assert_balance(OWNER, supply);

    state.mint(RECIPIENT, mint_amount);
    assert_total_supply(supply + mint_amount);
    assert_balance(RECIPIENT, mint_amount);
}

#[test]
#[fuzzer]
fn test_burn(supply: u256, burn_amount: u256) {
    if is_overflow_sub(supply, burn_amount) {
        return;
    }
    let mut state = setup(supply);

    assert_total_supply(supply);
    assert_balance(OWNER, supply);

    state.burn(OWNER, burn_amount);
    assert_total_supply(supply - burn_amount);
    assert_balance(OWNER, supply - burn_amount);
}

#[test]
#[fuzzer]
fn test_mint_burn(initial_supply: u256, mint_amount: u256, burn_amount: u256) {
    if is_overflow_add(initial_supply, mint_amount) {
        return;
    }
    if is_overflow_sub(mint_amount, burn_amount) {
        return;
    }
    let mut state = setup(initial_supply);
    let (owner, recipient) = (OWNER, RECIPIENT);

    // Mint
    state.mint(recipient, mint_amount);
    assert_total_supply(initial_supply + mint_amount);
    assert_balance(owner, initial_supply);
    assert_balance(recipient, mint_amount);

    // Burn
    state.burn(recipient, burn_amount);
    assert_total_supply(initial_supply + mint_amount - burn_amount);
    assert_balance(owner, initial_supply);
    assert_balance(recipient, mint_amount - burn_amount);
}

#[test]
#[fuzzer]
fn test_transfer(supply: u256, transfer_amount: u256) {
    if is_overflow_sub(supply, transfer_amount) {
        return;
    }
    let mut state = setup(supply);
    let (owner, recipient) = (OWNER, RECIPIENT);

    start_cheat_caller_address(test_address(), owner);
    state.transfer(recipient, transfer_amount);

    assert_balance(owner, supply - transfer_amount);
    assert_balance(recipient, transfer_amount);
}

#[test]
#[fuzzer]
fn test_transfer_from(supply: u256, transfer_amount: u256) {
    if is_overflow_sub(supply, transfer_amount) {
        return;
    }
    let mut state = setup(supply);
    let (owner, spender, recipient) = (OWNER, SPENDER, RECIPIENT);
    let contract_address = test_address();

    // Approve
    start_cheat_caller_address(contract_address, owner);
    state.approve(spender, transfer_amount);
    assert_balance(owner, supply);
    assert_allowance(owner, spender, transfer_amount);

    // Transfer from
    start_cheat_caller_address(contract_address, spender);
    state.transfer_from(owner, recipient, transfer_amount);
    assert_allowance(owner, spender, 0);
    assert_balance(owner, supply - transfer_amount);
    assert_balance(recipient, transfer_amount);
    assert_balance(spender, 0);
}

#[test]
#[fuzzer]
fn test__spend_allowance(supply: u256, spend_amount: u256) {
    if is_overflow_sub(supply, spend_amount) {
        return;
    }
    let mut state = setup(supply);
    let (owner, spender) = (OWNER, SPENDER);
    state._approve(owner, spender, supply);

    state._spend_allowance(owner, spender, spend_amount);

    // Allowance doesn't change if it's set to maximum
    let expected_allowance = if supply == Bounded::MAX {
        supply
    } else {
        supply - spend_amount
    };
    assert_balance(owner, supply);
    assert_balance(spender, 0);
    assert_allowance(owner, spender, expected_allowance);
}

//
// Helpers
//

fn assert_total_supply(expected: u256) {
    let state = COMPONENT_STATE();
    assert_eq!(state.total_supply(), expected);
}

fn assert_allowance(owner: ContractAddress, spender: ContractAddress, expected: u256) {
    let state = COMPONENT_STATE();
    assert_eq!(state.allowance(owner, spender), expected);
}

fn assert_balance(owner: ContractAddress, expected: u256) {
    let state = COMPONENT_STATE();
    assert_eq!(state.balance_of(owner), expected);
}
