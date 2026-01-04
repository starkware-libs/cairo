use core::num::traits::Bounded;
use openzeppelin_test_common::erc20::ERC20SpyHelpers;
use openzeppelin_test_common::mocks::erc20::{DualCaseERC20Mock, SnakeERC20MockWithHooks};
use openzeppelin_testing::constants::{NAME, OWNER, RECIPIENT, SPENDER, SUPPLY, SYMBOL, VALUE, ZERO};
use openzeppelin_testing::{EventSpyExt, EventSpyQueue as EventSpy, spy_events};
use snforge_std::{start_cheat_caller_address, test_address};
use starknet::ContractAddress;
use crate::erc20::ERC20Component;
use crate::erc20::ERC20Component::{ERC20CamelOnlyImpl, ERC20Impl, ERC20MetadataImpl, InternalImpl};

// Custom implementation of the ERC20Component ImmutableConfig used for testing.
impl ERC20ImmutableConfig of ERC20Component::ImmutableConfig {
    const DECIMALS: u8 = 6;
}

//
// Setup
//

type ComponentState = ERC20Component::ComponentState<DualCaseERC20Mock::ContractState>;
type ComponentStateWithHooks =
    ERC20Component::ComponentState<SnakeERC20MockWithHooks::ContractState>;

fn COMPONENT_STATE() -> ComponentState {
    ERC20Component::component_state_for_testing()
}

fn COMPONENT_STATE_WITH_HOOKS() -> ComponentStateWithHooks {
    ERC20Component::component_state_for_testing()
}

fn setup() -> ComponentState {
    let mut state = COMPONENT_STATE();
    state.initializer(NAME(), SYMBOL());
    state.mint(OWNER, SUPPLY);
    state
}

fn setup_with_hooks() -> ComponentStateWithHooks {
    let mut state = COMPONENT_STATE_WITH_HOOKS();
    state.initializer(NAME(), SYMBOL());
    state.mint(OWNER, SUPPLY);
    state
}

//
// initializer & constructor
//

#[test]
fn test_initializer() {
    let mut state = COMPONENT_STATE();
    state.initializer(NAME(), SYMBOL());

    assert_eq!(state.name(), NAME());
    assert_eq!(state.symbol(), SYMBOL());
    assert_eq!(state.decimals(), ERC20ImmutableConfig::DECIMALS);
    assert_eq!(state.total_supply(), 0);
}

//
// Getters
//

#[test]
fn test_total_supply() {
    let mut state = COMPONENT_STATE();
    state.mint(OWNER, SUPPLY);
    assert_eq!(state.total_supply(), SUPPLY);
}

#[test]
fn test_totalSupply() {
    let mut state = COMPONENT_STATE();
    state.mint(OWNER, SUPPLY);
    assert_eq!(state.totalSupply(), SUPPLY);
}

#[test]
fn test_balance_of() {
    let mut state = COMPONENT_STATE();
    state.mint(OWNER, SUPPLY);
    assert_eq!(state.balance_of(OWNER), SUPPLY);
}

#[test]
fn test_balanceOf() {
    let mut state = COMPONENT_STATE();
    state.mint(OWNER, SUPPLY);
    assert_eq!(state.balanceOf(OWNER), SUPPLY);
}

#[test]
fn test_allowance() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OWNER);
    state.approve(SPENDER, VALUE);

    let allowance = state.allowance(OWNER, SPENDER);
    assert_eq!(allowance, VALUE);
}

//
// approve & _approve
//

#[test]
fn test_approve() {
    let mut state = setup();
    let contract_address = test_address();
    let mut spy = spy_events();

    start_cheat_caller_address(contract_address, OWNER);
    let tx_success = state.approve(SPENDER, VALUE);
    assert!(tx_success);

    spy.assert_only_event_approval(contract_address, OWNER, SPENDER, VALUE);

    let allowance = state.allowance(OWNER, SPENDER);
    assert_eq!(allowance, VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: approve from 0')]
fn test_approve_from_zero() {
    let mut state = setup();
    state.approve(SPENDER, VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: approve to 0')]
fn test_approve_to_zero() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OWNER);
    state.approve(ZERO, VALUE);
}

#[test]
fn test__approve() {
    let mut state = setup();
    let contract_address = test_address();
    let mut spy = spy_events();

    start_cheat_caller_address(contract_address, OWNER);
    state._approve(OWNER, SPENDER, VALUE);

    spy.assert_only_event_approval(contract_address, OWNER, SPENDER, VALUE);

    let allowance = state.allowance(OWNER, SPENDER);
    assert_eq!(allowance, VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: approve from 0')]
fn test__approve_from_zero() {
    let mut state = setup();
    state._approve(ZERO, SPENDER, VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: approve to 0')]
fn test__approve_to_zero() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OWNER);
    state._approve(OWNER, ZERO, VALUE);
}

//
// transfer & _transfer
//

#[test]
fn test_transfer() {
    let mut state = setup();
    let contract_address = test_address();
    let mut spy = spy_events();

    start_cheat_caller_address(contract_address, OWNER);

    assert_state_before_transfer(OWNER, RECIPIENT);
    let tx_success = state.transfer(RECIPIENT, VALUE);
    assert!(tx_success);
    assert_state_after_transfer(OWNER, RECIPIENT, VALUE);

    spy.assert_only_event_transfer(contract_address, OWNER, RECIPIENT, VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: insufficient balance')]
fn test_transfer_not_enough_balance() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OWNER);

    let balance_plus_one = SUPPLY + 1;
    state.transfer(RECIPIENT, balance_plus_one);
}

#[test]
#[should_panic(expected: 'ERC20: transfer from 0')]
fn test_transfer_from_zero() {
    let mut state = setup();
    state.transfer(RECIPIENT, VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: transfer to 0')]
fn test_transfer_to_zero() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OWNER);
    state.transfer(ZERO, VALUE);
}

#[test]
fn test__transfer() {
    let mut state = setup();
    let contract_address = test_address();
    let mut spy = spy_events();

    assert_state_before_transfer(OWNER, RECIPIENT);
    state._transfer(OWNER, RECIPIENT, VALUE);
    assert_state_after_transfer(OWNER, RECIPIENT, VALUE);

    spy.assert_only_event_transfer(contract_address, OWNER, RECIPIENT, VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: insufficient balance')]
fn test__transfer_not_enough_balance() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OWNER);

    let balance_plus_one = SUPPLY + 1;
    state._transfer(OWNER, RECIPIENT, balance_plus_one);
}

#[test]
#[should_panic(expected: 'ERC20: transfer from 0')]
fn test__transfer_from_zero() {
    let mut state = setup();
    state._transfer(ZERO, RECIPIENT, VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: transfer to 0')]
fn test__transfer_to_zero() {
    let mut state = setup();
    state._transfer(OWNER, ZERO, VALUE);
}

//
// transfer_from & transferFrom
//

#[test]
fn test_transfer_from() {
    let mut state = setup();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, OWNER);
    state.approve(SPENDER, VALUE);

    let mut spy = spy_events();
    start_cheat_caller_address(contract_address, SPENDER);

    assert_state_before_transfer(OWNER, RECIPIENT);
    let tx_success = state.transfer_from(OWNER, RECIPIENT, VALUE);
    assert!(tx_success);
    assert_state_after_transfer(OWNER, RECIPIENT, VALUE);

    spy.assert_event_approval(contract_address, OWNER, SPENDER, 0);
    spy.assert_only_event_transfer(contract_address, OWNER, RECIPIENT, VALUE);

    let allowance = state.allowance(OWNER, SPENDER);
    assert_eq!(allowance, 0);
}

#[test]
fn test_transfer_from_doesnt_consume_infinite_allowance() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OWNER);
    state.approve(SPENDER, Bounded::MAX);

    start_cheat_caller_address(test_address(), SPENDER);
    state.transfer_from(OWNER, RECIPIENT, VALUE);

    let allowance = state.allowance(OWNER, SPENDER);
    assert_eq!(allowance, Bounded::MAX);
}

#[test]
#[should_panic(expected: 'ERC20: insufficient allowance')]
fn test_transfer_from_greater_than_allowance() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OWNER);
    state.approve(SPENDER, VALUE);

    start_cheat_caller_address(test_address(), SPENDER);
    let allowance_plus_one = VALUE + 1;
    state.transfer_from(OWNER, RECIPIENT, allowance_plus_one);
}

#[test]
#[should_panic(expected: 'ERC20: transfer to 0')]
fn test_transfer_from_to_zero_address() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OWNER);
    state.approve(SPENDER, VALUE);

    start_cheat_caller_address(test_address(), SPENDER);
    state.transfer_from(OWNER, ZERO, VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: insufficient allowance')]
fn test_transfer_from_from_zero_address() {
    let mut state = setup();
    state.transfer_from(ZERO, RECIPIENT, VALUE);
}

#[test]
fn test_transferFrom() {
    let mut state = setup();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, OWNER);
    state.approve(SPENDER, VALUE);

    let mut spy = spy_events();
    start_cheat_caller_address(contract_address, SPENDER);

    assert_state_before_transfer(OWNER, RECIPIENT);
    let tx_success = state.transferFrom(OWNER, RECIPIENT, VALUE);
    assert!(tx_success);
    assert_state_after_transfer(OWNER, RECIPIENT, VALUE);

    spy.assert_event_approval(contract_address, OWNER, SPENDER, 0);
    spy.assert_only_event_transfer(contract_address, OWNER, RECIPIENT, VALUE);

    let allowance = state.allowance(OWNER, SPENDER);
    assert_eq!(allowance, 0);
}

#[test]
fn test_transferFrom_doesnt_consume_infinite_allowance() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OWNER);
    state.approve(SPENDER, Bounded::MAX);

    start_cheat_caller_address(test_address(), SPENDER);
    state.transferFrom(OWNER, RECIPIENT, VALUE);

    let allowance = state.allowance(OWNER, SPENDER);
    assert_eq!(allowance, Bounded::MAX);
}

#[test]
#[should_panic(expected: 'ERC20: insufficient allowance')]
fn test_transferFrom_greater_than_allowance() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OWNER);
    state.approve(SPENDER, VALUE);

    start_cheat_caller_address(test_address(), SPENDER);
    let allowance_plus_one = VALUE + 1;
    state.transferFrom(OWNER, RECIPIENT, allowance_plus_one);
}

#[test]
#[should_panic(expected: 'ERC20: transfer to 0')]
fn test_transferFrom_to_zero_address() {
    let mut state = setup();
    start_cheat_caller_address(test_address(), OWNER);
    state.approve(SPENDER, VALUE);

    start_cheat_caller_address(test_address(), SPENDER);
    state.transferFrom(OWNER, ZERO, VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: insufficient allowance')]
fn test_transferFrom_from_zero_address() {
    let mut state = setup();
    state.transferFrom(ZERO, RECIPIENT, VALUE);
}

//
// _spend_allowance
//

#[test]
fn test__spend_allowance_not_unlimited() {
    let mut state = setup();
    let contract_address = test_address();

    state._approve(OWNER, SPENDER, SUPPLY);

    let mut spy = spy_events();
    state._spend_allowance(OWNER, SPENDER, VALUE);

    spy.assert_only_event_approval(contract_address, OWNER, SPENDER, SUPPLY - VALUE);

    let allowance = state.allowance(OWNER, SPENDER);
    assert_eq!(allowance, SUPPLY - VALUE);
}

#[test]
fn test__spend_allowance_unlimited() {
    let mut state = setup();
    state._approve(OWNER, SPENDER, Bounded::MAX);

    let max_minus_one: u256 = Bounded::MAX - 1;
    state._spend_allowance(OWNER, SPENDER, max_minus_one);

    let allowance = state.allowance(OWNER, SPENDER);
    assert_eq!(allowance, Bounded::MAX);
}

//
// mint
//

#[test]
fn test_mint() {
    let mut state = setup();
    let contract_address = test_address();

    let mut spy = spy_events();

    assert_state_before_mint(RECIPIENT);
    state.mint(RECIPIENT, VALUE);
    assert_state_after_mint(RECIPIENT, VALUE);

    spy.assert_only_event_transfer(contract_address, ZERO, RECIPIENT, VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: mint to 0')]
fn test_mint_to_zero() {
    let mut state = COMPONENT_STATE();
    state.mint(ZERO, VALUE);
}

//
// burn
//

#[test]
fn test_burn() {
    let mut state = setup();
    let contract_address = test_address();

    let mut spy = spy_events();

    assert_state_before_burn(OWNER);
    state.burn(OWNER, VALUE);
    assert_state_after_burn(OWNER, VALUE);

    spy.assert_only_event_transfer(contract_address, OWNER, ZERO, VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: insufficient balance')]
fn test_burn_insufficient_balance() {
    let mut state = setup();
    let overflow_amt = SUPPLY + 1;

    state.burn(OWNER, overflow_amt);
}

#[test]
#[should_panic(expected: 'ERC20: burn from 0')]
fn test_burn_from_zero() {
    let mut state = setup();
    state.burn(ZERO, VALUE);
}

//
// update
//

#[test]
fn test_update_from_non_zero_to_non_zero() {
    let mut state = setup();

    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, OWNER);

    assert_state_before_transfer(OWNER, RECIPIENT);
    state.update(OWNER, RECIPIENT, VALUE);
    assert_state_after_transfer(OWNER, RECIPIENT, VALUE);

    spy.assert_only_event_transfer(contract_address, OWNER, RECIPIENT, VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: insufficient balance')]
fn test_update_from_non_zero_to_non_zero_insufficient_balance() {
    let mut state = setup();
    let contract_address = test_address();
    let overflow_amt = SUPPLY + 1;

    start_cheat_caller_address(contract_address, OWNER);
    state.update(OWNER, RECIPIENT, overflow_amt);
}

#[test]
fn test_update_from_non_zero_to_zero() {
    let mut state = setup();

    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, OWNER);

    assert_state_before_burn(OWNER);
    state.update(OWNER, ZERO, VALUE);
    assert_state_after_burn(OWNER, VALUE);

    spy.assert_only_event_transfer(contract_address, OWNER, ZERO, VALUE);
}

#[test]
#[should_panic(expected: 'ERC20: insufficient balance')]
fn test_update_from_non_zero_to_zero_insufficient_balance() {
    let mut state = setup();
    let contract_address = test_address();
    let overflow_amt = SUPPLY + 1;

    start_cheat_caller_address(contract_address, OWNER);
    state.update(OWNER, ZERO, overflow_amt);
}

#[test]
fn test_update_from_zero_to_non_zero() {
    let mut state = setup();

    let mut spy = spy_events();
    let contract_address = test_address();

    start_cheat_caller_address(contract_address, RECIPIENT);

    assert_state_before_mint(RECIPIENT);
    state.update(ZERO, RECIPIENT, VALUE);
    assert_state_after_mint(RECIPIENT, VALUE);

    spy.assert_only_event_transfer(contract_address, ZERO, RECIPIENT, VALUE);
}

#[test]
fn test_update_from_zero_to_zero() {
    // The update function should not be called from zero to zero
    // because this does nothing to the state.
    let mut state = setup();

    let mut spy = spy_events();
    let contract_address = test_address();
    let supply_before_update = state.total_supply();

    start_cheat_caller_address(contract_address, RECIPIENT);

    state.update(ZERO, ZERO, VALUE);

    let supply_after_update = state.total_supply();
    assert_eq!(supply_before_update, supply_after_update);

    spy.assert_only_event_transfer(contract_address, ZERO, ZERO, VALUE);
}

#[test]
fn test_update_calls_before_update_hook() {
    let mut state = setup_with_hooks();

    let mut spy = spy_events();
    let contract_address = test_address();

    state.update(OWNER, RECIPIENT, VALUE);

    spy.assert_event_before_update(contract_address, OWNER, RECIPIENT, VALUE);
}

#[test]
fn test_update_calls_after_update_hook() {
    let mut state = setup_with_hooks();

    let mut spy = spy_events();
    let contract_address = test_address();

    state.update(OWNER, RECIPIENT, VALUE);

    spy.assert_event_after_update(contract_address, OWNER, RECIPIENT, VALUE);
}

#[test]
fn test_default_config() {
    let decimals = crate::erc20::DefaultConfig::DECIMALS;
    assert_eq!(decimals, ERC20Component::DEFAULT_DECIMALS);
    assert_eq!(decimals, 18);
}

//
// Helpers
//

fn assert_state_before_transfer(sender: ContractAddress, recipient: ContractAddress) {
    let state = COMPONENT_STATE();
    let initial_supply = SUPPLY;
    let current_supply = state.total_supply();

    assert_eq!(initial_supply, current_supply);
    assert_eq!(state.balance_of(sender), SUPPLY);
    assert_eq!(state.balance_of(recipient), 0);
}

fn assert_state_after_transfer(sender: ContractAddress, recipient: ContractAddress, amount: u256) {
    let state = COMPONENT_STATE();
    let initial_supply = SUPPLY;
    let current_supply = state.total_supply();

    assert_eq!(initial_supply, current_supply);
    assert_eq!(state.balance_of(sender), initial_supply - amount);
    assert_eq!(state.balance_of(recipient), amount);
}

fn assert_state_before_mint(recipient: ContractAddress) {
    let state = COMPONENT_STATE();
    let initial_supply = SUPPLY;
    let current_supply = state.total_supply();

    assert_eq!(current_supply, initial_supply);
    assert_eq!(state.balance_of(recipient), 0);
}

fn assert_state_after_mint(recipient: ContractAddress, amount: u256) {
    let state = COMPONENT_STATE();
    let initial_supply = SUPPLY;
    let current_supply = state.total_supply();

    assert_eq!(current_supply, initial_supply + amount);
    assert_eq!(state.balance_of(recipient), amount);
}

fn assert_state_before_burn(account: ContractAddress) {
    let state = COMPONENT_STATE();
    let initial_supply = SUPPLY;
    let current_supply = state.total_supply();

    assert_eq!(initial_supply, current_supply);
    assert_eq!(state.balance_of(account), SUPPLY);
}

fn assert_state_after_burn(account: ContractAddress, amount: u256) {
    let state = COMPONENT_STATE();
    let initial_supply = SUPPLY;
    let current_supply = state.total_supply();

    assert_eq!(current_supply, initial_supply - amount);
    assert_eq!(state.balance_of(account), initial_supply - amount);
}

#[generate_trait]
impl ERC20HooksSpyHelpersImpl of ERC20HooksSpyHelpers {
    fn assert_event_before_update(
        ref self: EventSpy,
        contract: ContractAddress,
        from: ContractAddress,
        recipient: ContractAddress,
        amount: u256,
    ) {
        let expected = SnakeERC20MockWithHooks::Event::BeforeUpdate(
            SnakeERC20MockWithHooks::BeforeUpdate { from, recipient, amount },
        );
        self.assert_emitted_single(contract, expected);
    }

    fn assert_event_after_update(
        ref self: EventSpy,
        contract: ContractAddress,
        from: ContractAddress,
        recipient: ContractAddress,
        amount: u256,
    ) {
        let expected = SnakeERC20MockWithHooks::Event::AfterUpdate(
            SnakeERC20MockWithHooks::AfterUpdate { from, recipient, amount },
        );
        self.assert_emitted_single(contract, expected);
    }
}
