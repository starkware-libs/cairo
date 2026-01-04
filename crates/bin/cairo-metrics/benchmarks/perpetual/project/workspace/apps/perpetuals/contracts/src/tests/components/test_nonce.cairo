use core::num::traits::Zero;
use perpetuals::core::components::operator_nonce::OperatorNonceComponent;
use perpetuals::core::components::operator_nonce::OperatorNonceComponent::InternalTrait;
use perpetuals::core::components::operator_nonce::interface::IOperatorNonce;
use perpetuals::tests::components::mock_nonce::NonceMock;
use snforge_std::test_address;
use starknet::ContractAddress;
use starkware_utils::components::roles::RolesComponent::InternalTrait as RolesInternal;
use starkware_utils::components::roles::interface::IRoles;
use starkware_utils_testing::test_utils::cheat_caller_address_once;

type ComponentState = OperatorNonceComponent::ComponentState<NonceMock::ContractState>;

const OPERATOR: ContractAddress = 'OPERATOR'.try_into().unwrap();

fn CONTRACT_STATE() -> NonceMock::ContractState {
    let mut state = NonceMock::contract_state_for_testing();
    state.roles.initialize(OPERATOR);
    cheat_caller_address_once(contract_address: test_address(), caller_address: OPERATOR);
    state.roles.register_app_role_admin(OPERATOR);
    cheat_caller_address_once(contract_address: test_address(), caller_address: OPERATOR);
    state.roles.register_operator(OPERATOR);
    state
}

#[test]
fn test_nonce_getter() {
    let state = CONTRACT_STATE();
    let nonce = state.get_operator_nonce();
    assert!(nonce.is_zero());
}

#[test]
fn test_use_checked_nonce() {
    let mut state = CONTRACT_STATE();
    cheat_caller_address_once(contract_address: test_address(), caller_address: OPERATOR);
    let nonce = state.nonce.use_checked_nonce(0);
    assert!(nonce.is_zero());
    let nonce = state.get_operator_nonce();
    assert_eq!(nonce, 1, "use_checked_nonce should increment the nonce by 1");
}

#[test]
#[should_panic(expected: "INVALID_NONCE: current!=received 0!=15")]
fn test_use_checked_nonce_invalid_current() {
    let mut state = CONTRACT_STATE();
    cheat_caller_address_once(contract_address: test_address(), caller_address: OPERATOR);
    state.nonce.use_checked_nonce(15);
}
