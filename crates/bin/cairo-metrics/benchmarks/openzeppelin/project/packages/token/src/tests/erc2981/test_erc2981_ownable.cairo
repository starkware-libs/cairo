use openzeppelin_access::ownable::OwnableComponent;
use openzeppelin_access::ownable::OwnableComponent::InternalImpl as OwnableInternalImpl;
use openzeppelin_test_common::mocks::erc2981::ERC2981OwnableMock;
use openzeppelin_testing::constants::{OTHER, OWNER, RECIPIENT, ZERO};
use snforge_std::{start_cheat_caller_address, test_address};
use starknet::ContractAddress;
use crate::common::erc2981::ERC2981Component::{
    ERC2981AdminOwnableImpl, ERC2981Impl, ERC2981InfoImpl, InternalImpl,
};
use crate::common::erc2981::{DefaultConfig, ERC2981Component};

type MockState = ERC2981OwnableMock::ContractState;
type ComponentState = ERC2981Component::ComponentState<MockState>;
type OwnableComponentState = OwnableComponent::ComponentState<MockState>;

fn COMPONENT_STATE() -> ComponentState {
    ERC2981Component::component_state_for_testing()
}

const DEFAULT_RECEIVER: ContractAddress = 'DEFAULT_RECEIVER'.try_into().unwrap();

// 0.5% (default denominator is 10000)
const DEFAULT_FEE_NUMERATOR: u128 = 50;
// 5% (default denominator is 10000)
const FEE_NUMERATOR: u128 = 500;

fn setup() -> ComponentState {
    let mut state = COMPONENT_STATE();
    state.initializer(DEFAULT_RECEIVER, DEFAULT_FEE_NUMERATOR);

    let mut ownable_state: OwnableComponentState = OwnableComponent::component_state_for_testing();
    ownable_state.initializer(OWNER);

    state
}

//
// IERC2981Info
//

#[test]
fn test_default_royalty() {
    let state = setup();

    let (receiver, numerator, denominator) = state.default_royalty();

    assert_eq!(receiver, DEFAULT_RECEIVER);
    assert_eq!(numerator, DEFAULT_FEE_NUMERATOR);
    assert_eq!(denominator, ERC2981Component::DEFAULT_FEE_DENOMINATOR);
}

#[test]
fn test_royalty_info_default_royalty() {
    let state = setup();
    let token_id = 12;
    let sale_price = 1_000_000;

    let (receiver, amount) = state.royalty_info(token_id, sale_price);
    assert_eq!(receiver, DEFAULT_RECEIVER);
    assert_eq!(amount, 5000);
}

//
// IERC2981Admin
//

#[test]
fn test_royalty_info_token_royalty_set() {
    let mut state = setup();
    let token_id = 12;
    let sale_price = 1_000_000;

    let (receiver, amount) = state.royalty_info(token_id, sale_price);
    assert_eq!(receiver, DEFAULT_RECEIVER);
    assert_eq!(amount, 5_000);

    start_cheat_caller_address(test_address(), OWNER);
    state.set_token_royalty(token_id, RECIPIENT, FEE_NUMERATOR);

    let (receiver, amount) = state.royalty_info(token_id, sale_price);
    assert_eq!(receiver, RECIPIENT);
    assert_eq!(amount, 50_000);
}

#[test]
fn test_set_default_royalty() {
    let mut state = setup();
    let token_id = 12;
    let sale_price = 1_000_000;

    let (receiver, amount) = state.royalty_info(token_id, sale_price);
    assert_eq!(receiver, DEFAULT_RECEIVER);
    assert_eq!(amount, 5_000);

    start_cheat_caller_address(test_address(), OWNER);
    state.set_default_royalty(RECIPIENT, FEE_NUMERATOR);

    let (receiver, amount) = state.royalty_info(token_id, sale_price);
    assert_eq!(receiver, RECIPIENT);
    assert_eq!(amount, 50_000);
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_set_default_royalty_unauthorized() {
    let mut state = setup();

    start_cheat_caller_address(test_address(), OTHER);
    state.set_default_royalty(RECIPIENT, FEE_NUMERATOR);
}

#[test]
fn test_set_default_royalty_with_zero_royalty_fraction() {
    let mut state = setup();
    let token_id = 12;
    let sale_price = 1_000_000;

    start_cheat_caller_address(test_address(), OWNER);
    state.set_default_royalty(DEFAULT_RECEIVER, 0);

    let (receiver, amount) = state.royalty_info(token_id, sale_price);
    assert_eq!(receiver, DEFAULT_RECEIVER);
    assert_eq!(amount, 0);
}

#[test]
#[should_panic(expected: 'ERC2981: invalid receiver')]
fn test_set_default_royalty_with_zero_receiver() {
    let mut state = setup();

    start_cheat_caller_address(test_address(), OWNER);
    state.set_default_royalty(ZERO, FEE_NUMERATOR);
}

#[test]
#[should_panic(expected: 'ERC2981: invalid royalty')]
fn test_set_default_royalty_with_invalid_fee_numerator() {
    let mut state = setup();
    let fee_denominator = ERC2981Component::DEFAULT_FEE_DENOMINATOR;

    start_cheat_caller_address(test_address(), OWNER);
    state.set_default_royalty(DEFAULT_RECEIVER, fee_denominator + 1);
}

#[test]
fn test_delete_default_royalty() {
    let mut state = setup();
    let token_id = 12;
    let sale_price = 1_000_000;

    start_cheat_caller_address(test_address(), OWNER);
    state.set_default_royalty(RECIPIENT, FEE_NUMERATOR);

    let (receiver, amount) = state.royalty_info(token_id, sale_price);
    assert_eq!(receiver, RECIPIENT);
    assert_eq!(amount, 50_000);

    start_cheat_caller_address(test_address(), OWNER);
    state.delete_default_royalty();

    let (receiver, amount) = state.royalty_info(token_id, sale_price);
    assert_eq!(receiver, ZERO);
    assert_eq!(amount, 0);
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_delete_default_royalty_unauthorized() {
    let mut state = setup();

    start_cheat_caller_address(test_address(), OTHER);
    state.delete_default_royalty();
}

#[test]
fn test_set_token_royalty() {
    let mut state = setup();
    let token_id = 12;
    let another_token_id = 13;
    let sale_price = 1_000_000;

    let (receiver, amount) = state.royalty_info(token_id, sale_price);
    assert_eq!(receiver, DEFAULT_RECEIVER);
    assert_eq!(amount, 5_000);

    let (receiver, amount) = state.royalty_info(another_token_id, sale_price);
    assert_eq!(receiver, DEFAULT_RECEIVER);
    assert_eq!(amount, 5_000);

    start_cheat_caller_address(test_address(), OWNER);
    state.set_token_royalty(token_id, RECIPIENT, FEE_NUMERATOR);

    let (receiver, amount) = state.royalty_info(token_id, sale_price);
    assert_eq!(receiver, RECIPIENT);
    assert_eq!(amount, 50_000);

    let (receiver, amount) = state.royalty_info(another_token_id, sale_price);
    assert_eq!(receiver, DEFAULT_RECEIVER);
    assert_eq!(amount, 5_000);
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_set_token_royalty_unauthorized() {
    let mut state = setup();
    let token_id = 12;

    start_cheat_caller_address(test_address(), OTHER);
    state.set_token_royalty(token_id, RECIPIENT, FEE_NUMERATOR);
}

#[test]
fn test_set_token_royalty_with_zero_royalty_fraction() {
    let mut state = setup();
    let token_id = 12;
    let sale_price = 1_000_000;

    start_cheat_caller_address(test_address(), OWNER);
    state.set_token_royalty(token_id, RECIPIENT, 0);

    let (receiver, amount) = state.royalty_info(token_id, sale_price);
    assert_eq!(receiver, RECIPIENT);
    assert_eq!(amount, 0);
}

#[test]
#[should_panic(expected: 'ERC2981: invalid receiver')]
fn test_set_token_royalty_with_zero_receiver() {
    let mut state = setup();
    let token_id = 12;

    start_cheat_caller_address(test_address(), OWNER);
    state.set_token_royalty(token_id, ZERO, FEE_NUMERATOR);
}

#[test]
#[should_panic(expected: 'ERC2981: invalid royalty')]
fn test_set_token_royalty_with_invalid_fee_numerator() {
    let mut state = setup();
    let token_id = 12;
    let fee_denominator = ERC2981Component::DEFAULT_FEE_DENOMINATOR;

    start_cheat_caller_address(test_address(), OWNER);
    state.set_token_royalty(token_id, RECIPIENT, fee_denominator + 1);
}

#[test]
fn test_reset_token_royalty() {
    let mut state = setup();
    let token_id = 12;
    let sale_price = 1_000_000;

    let (receiver, amount) = state.royalty_info(token_id, sale_price);
    assert_eq!(receiver, DEFAULT_RECEIVER);
    assert_eq!(amount, 5_000);

    start_cheat_caller_address(test_address(), OWNER);
    state.set_token_royalty(token_id, RECIPIENT, FEE_NUMERATOR);

    let (receiver, amount) = state.royalty_info(token_id, sale_price);
    assert_eq!(receiver, RECIPIENT);
    assert_eq!(amount, 50_000);

    start_cheat_caller_address(test_address(), OWNER);
    state.reset_token_royalty(token_id);

    let (receiver, amount) = state.royalty_info(token_id, sale_price);
    assert_eq!(receiver, DEFAULT_RECEIVER);
    assert_eq!(amount, 5_000);
}

#[test]
#[should_panic(expected: 'Caller is not the owner')]
fn test_reset_token_royalty_unauthorized() {
    let mut state = setup();
    let token_id = 12;

    start_cheat_caller_address(test_address(), OTHER);
    state.reset_token_royalty(token_id);
}
