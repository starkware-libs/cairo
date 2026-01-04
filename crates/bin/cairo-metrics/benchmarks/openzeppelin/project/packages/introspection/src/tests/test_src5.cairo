use openzeppelin_test_common::mocks::src5::SRC5Mock;
use crate::interface::{ISRC5, ISRC5_ID};
use crate::src5::SRC5Component;
use crate::src5::SRC5Component::InternalTrait;

const OTHER_ID: felt252 = 0x12345678;

type ComponentState = SRC5Component::ComponentState<SRC5Mock::ContractState>;

fn COMPONENT_STATE() -> ComponentState {
    SRC5Component::component_state_for_testing()
}

#[test]
fn test_default_behavior() {
    let state = COMPONENT_STATE();
    let supports_isrc5 = state.supports_interface(ISRC5_ID);
    assert!(supports_isrc5);
}

#[test]
fn test_not_registered_interface() {
    let state = COMPONENT_STATE();
    let does_not_support_unregistered_interface = !state.supports_interface(OTHER_ID);
    assert!(does_not_support_unregistered_interface);
}

#[test]
fn test_register_interface() {
    let mut state = COMPONENT_STATE();
    state.register_interface(OTHER_ID);
    let supports_new_interface = state.supports_interface(OTHER_ID);
    assert!(supports_new_interface);
}

#[test]
fn test_deregister_interface() {
    let mut state = COMPONENT_STATE();
    state.register_interface(OTHER_ID);
    state.deregister_interface(OTHER_ID);
    let does_not_support_old_interface = !state.supports_interface(OTHER_ID);
    assert!(does_not_support_old_interface);
}

#[test]
#[should_panic(expected: 'SRC5: invalid id')]
fn test_deregister_default_interface() {
    let mut state = COMPONENT_STATE();
    state.deregister_interface(ISRC5_ID);
}
