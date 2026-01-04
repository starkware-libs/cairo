use openzeppelin_test_common::mocks::security::InitializableMock;
use crate::InitializableComponent;
use crate::InitializableComponent::{InitializableImpl, InternalImpl};

type ComponentState = InitializableComponent::ComponentState<InitializableMock::ContractState>;

fn COMPONENT_STATE() -> ComponentState {
    InitializableComponent::component_state_for_testing()
}

#[test]
fn test_initialize() {
    let mut state = COMPONENT_STATE();
    assert!(!state.is_initialized());
    state.initialize();
    assert!(state.is_initialized());
}

#[test]
#[should_panic(expected: 'Initializable: is initialized')]
fn test_initialize_when_initialized() {
    let mut state = COMPONENT_STATE();
    state.initialize();
    state.initialize();
}
