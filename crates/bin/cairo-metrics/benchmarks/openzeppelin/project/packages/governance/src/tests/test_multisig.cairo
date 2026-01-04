use core::integer::u128_safe_divmod;
use core::num::traits::{Bounded, Zero};
use openzeppelin_test_common::mocks::multisig::{
    IMultisigTargetMockDispatcher, IMultisigTargetMockDispatcherTrait, MultisigWalletMock,
};
use openzeppelin_testing as utils;
use openzeppelin_testing::constants::{ALICE, BLOCK_NUMBER, BOB, CHARLIE, OTHER, SALT, ZERO};
use openzeppelin_testing::{EventSpyExt, EventSpyQueue as EventSpy, spy_events};
use snforge_std::{start_cheat_block_number_global, start_cheat_caller_address, test_address};
use starknet::ContractAddress;
use starknet::account::Call;
use starknet::storage_access::StorePacking;
use crate::multisig::MultisigComponent::{
    CallSalt, ConfirmationRevoked, Event, InternalImpl, MultisigImpl, QuorumUpdated, SignerAdded,
    SignerRemoved, TransactionConfirmed, TransactionExecuted, TransactionSubmitted,
};
use crate::multisig::storage_utils::{SignersInfo, SignersInfoStorePackingV2};
use crate::multisig::{MultisigComponent, TransactionID, TransactionState};

//
// Setup
//

type ComponentState = MultisigComponent::ComponentState<MultisigWalletMock::ContractState>;

fn COMPONENT_STATE() -> ComponentState {
    MultisigComponent::component_state_for_testing()
}

fn DEFAULT_DATA() -> (u32, Span<ContractAddress>) {
    let signers = array![ALICE, BOB, CHARLIE];
    let quorum = signers.len() - 1;
    (quorum, signers.span())
}

const MOCK_ADDRESS: ContractAddress = 'MOCK_ADDRESS'.try_into().unwrap();

fn setup_component(quorum: u32, signers: Span<ContractAddress>) -> ComponentState {
    start_cheat_block_number_global(BLOCK_NUMBER);
    let mut state = COMPONENT_STATE();
    state.initializer(quorum, signers);
    state
}

fn deploy_mock() -> IMultisigTargetMockDispatcher {
    let contract_address = MOCK_ADDRESS;
    utils::declare_and_deploy_at("MultisigTargetMock", contract_address, array![]);
    IMultisigTargetMockDispatcher { contract_address }
}

//
// Initializer
//

#[test]
#[should_panic(expected: 'Multisig: quorum cannot be 0')]
fn test_init_zero_quorum() {
    let (_, signers) = DEFAULT_DATA();
    let quorum = 0;
    setup_component(quorum, signers);
}

//
// Submit tx
//

#[test]
fn test_submit_tx() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let mut spy = spy_events();
    let contract_address = test_address();

    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    let salt = 0;
    let expected_id = state.hash_transaction(to, selector, calldata, salt);
    assert_tx_state(expected_id, TransactionState::NotFound);

    let signer = ALICE;
    start_cheat_caller_address(contract_address, signer);

    let id = state.submit_transaction(to, selector, calldata, salt);
    assert_eq!(id, expected_id);
    assert_eq!(state.get_submitted_block(id), BLOCK_NUMBER);
    assert_tx_state(id, TransactionState::Pending);
    spy.assert_only_event_tx_submitted(contract_address, id, signer);
}

#[test]
fn test_submit_tx_with_salt() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let mut spy = spy_events();
    let contract_address = test_address();

    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    let salt = SALT;
    let expected_id = state.hash_transaction(to, selector, calldata, salt);
    assert_tx_state(expected_id, TransactionState::NotFound);

    let signer = ALICE;
    start_cheat_caller_address(contract_address, signer);

    let id = state.submit_transaction(to, selector, calldata, salt);
    assert_eq!(id, expected_id);
    assert_eq!(state.get_submitted_block(id), BLOCK_NUMBER);
    assert_tx_state(id, TransactionState::Pending);
    spy.assert_event_call_salt(contract_address, id, salt);
    spy.assert_event_tx_submitted(contract_address, id, signer);
}

#[test]
fn test_submit_same_tx_again_different_salt() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let mut spy = spy_events();
    let contract_address = test_address();

    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    let salt_1 = 0;
    let expected_id_1 = state.hash_transaction(to, selector, calldata, salt_1);
    let salt_2 = SALT;
    let expected_id_2 = state.hash_transaction(to, selector, calldata, salt_2);
    assert!(expected_id_1 != expected_id_2);

    let signer = ALICE;
    start_cheat_caller_address(contract_address, signer);

    let id_1 = state.submit_transaction(to, selector, calldata, salt_1);
    assert_eq!(id_1, expected_id_1);
    assert_tx_state(id_1, TransactionState::Pending);
    spy.assert_only_event_tx_submitted(contract_address, id_1, signer);

    let id_2 = state.submit_transaction(to, selector, calldata, salt_2);
    assert_eq!(id_2, expected_id_2);
    assert_tx_state(id_2, TransactionState::Pending);
    spy.assert_event_call_salt(contract_address, id_2, salt_2);
    spy.assert_only_event_tx_submitted(contract_address, id_2, signer);
}

#[test]
fn test_submit_tx_batch() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let mut spy = spy_events();
    let contract_address = test_address();

    let calls = array![
        build_call(MockCall::AddNumber(42)),
        build_call(MockCall::AddNumber(18)),
        build_call(MockCall::AddNumber(40)),
    ]
        .span();
    let salt = 0;
    let expected_id = state.hash_transaction_batch(calls, salt);
    assert_tx_state(expected_id, TransactionState::NotFound);

    let signer = ALICE;
    start_cheat_caller_address(contract_address, signer);

    let id = state.submit_transaction_batch(calls, salt);
    assert_eq!(id, expected_id);
    assert_eq!(state.get_submitted_block(id), BLOCK_NUMBER);
    assert_tx_state(id, TransactionState::Pending);
    spy.assert_only_event_tx_submitted(contract_address, id, signer);
}

#[test]
fn test_submit_tx_batch_with_salt() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let mut spy = spy_events();
    let contract_address = test_address();

    let calls = array![
        build_call(MockCall::AddNumber(42)),
        build_call(MockCall::AddNumber(18)),
        build_call(MockCall::AddNumber(40)),
    ]
        .span();
    let salt = SALT;
    let expected_id = state.hash_transaction_batch(calls, salt);
    assert_tx_state(expected_id, TransactionState::NotFound);

    let signer = ALICE;
    start_cheat_caller_address(contract_address, signer);

    let id = state.submit_transaction_batch(calls, salt);
    assert_eq!(id, expected_id);
    assert_eq!(state.get_submitted_block(id), BLOCK_NUMBER);
    assert_tx_state(id, TransactionState::Pending);
    spy.assert_event_call_salt(contract_address, id, salt);
    spy.assert_event_tx_submitted(contract_address, id, signer);
}

#[test]
fn test_submit_same_tx_batch_different_salt() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let mut spy = spy_events();
    let contract_address = test_address();

    let calls = array![
        build_call(MockCall::AddNumber(42)),
        build_call(MockCall::AddNumber(18)),
        build_call(MockCall::AddNumber(40)),
    ]
        .span();
    let salt_1 = 0;
    let expected_id_1 = state.hash_transaction_batch(calls, salt_1);
    let salt_2 = SALT;
    let expected_id_2 = state.hash_transaction_batch(calls, salt_2);
    assert!(expected_id_1 != expected_id_2);

    let signer = ALICE;
    start_cheat_caller_address(contract_address, signer);

    let id_1 = state.submit_transaction_batch(calls, salt_1);
    assert_eq!(id_1, expected_id_1);
    assert_tx_state(id_1, TransactionState::Pending);
    spy.assert_only_event_tx_submitted(contract_address, id_1, signer);

    let id_2 = state.submit_transaction_batch(calls, salt_2);
    assert_eq!(id_2, expected_id_2);
    assert_tx_state(id_2, TransactionState::Pending);
    spy.assert_event_call_salt(contract_address, id_2, salt_2);
    spy.assert_event_tx_submitted(contract_address, id_2, signer);
}

#[test]
#[should_panic(expected: 'Multisig: not a signer')]
fn test_cannot_submit_tx_unauthorized() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);

    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    let signer = OTHER;
    start_cheat_caller_address(test_address(), signer);
    state.submit_transaction(to, selector, calldata, 0);
}

#[test]
#[should_panic(expected: 'Multisig: not a signer')]
fn test_cannot_submit_tx_batch_unauthorized() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);

    let calls = array![
        build_call(MockCall::AddNumber(42)),
        build_call(MockCall::AddNumber(18)),
        build_call(MockCall::AddNumber(40)),
    ]
        .span();
    let signer = OTHER;
    start_cheat_caller_address(test_address(), signer);
    state.submit_transaction_batch(calls, 0);
}

#[test]
#[should_panic(expected: 'Multisig: tx already exists')]
fn test_cannot_submit_tx_twice() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);

    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    let signer = ALICE;
    start_cheat_caller_address(test_address(), signer);
    state.submit_transaction(to, selector, calldata, 0);
    state.submit_transaction(to, selector, calldata, 0);
}

#[test]
#[should_panic(expected: 'Multisig: tx already exists')]
fn test_cannot_submit_tx_batch_twice() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);

    let calls = array![
        build_call(MockCall::AddNumber(42)),
        build_call(MockCall::AddNumber(18)),
        build_call(MockCall::AddNumber(40)),
    ]
        .span();
    let signer = ALICE;
    start_cheat_caller_address(test_address(), signer);
    state.submit_transaction_batch(calls, 0);
    state.submit_transaction_batch(calls, 0);
}

//
// Confirm tx
//

#[test]
fn test_confirm_tx() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let mut spy = spy_events();
    let contract_address = test_address();
    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));

    // Submit by Alice
    start_cheat_caller_address(contract_address, ALICE);
    let id = state.submit_transaction(to, selector, calldata, 0);

    // Confirm by Bob
    spy.drop_all_events();
    start_cheat_caller_address(contract_address, BOB);
    assert_eq!(state.is_confirmed_by(id, BOB), false);
    state.confirm_transaction(id);
    assert_eq!(state.is_confirmed_by(id, BOB), true);
    assert_tx_state(id, TransactionState::Pending);
    assert_eq!(state.get_transaction_confirmations(id), 1);
    spy.assert_only_event_tx_confirmed(contract_address, id, BOB);

    // Confirm by Charlie
    start_cheat_caller_address(contract_address, CHARLIE);
    assert_eq!(state.is_confirmed_by(id, CHARLIE), false);
    state.confirm_transaction(id);
    assert_eq!(state.is_confirmed_by(id, CHARLIE), true);
    assert_tx_state(id, TransactionState::Confirmed);
    assert_eq!(state.get_transaction_confirmations(id), 2);
    spy.assert_only_event_tx_confirmed(contract_address, id, CHARLIE);
}

#[test]
fn test_confirmed_status_changed_when_quorum_increased() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let contract_address = test_address();
    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));

    // Submit by Alice
    start_cheat_caller_address(contract_address, ALICE);
    let id = state.submit_transaction(to, selector, calldata, 0);

    // Confirm by Bob
    start_cheat_caller_address(contract_address, BOB);
    state.confirm_transaction(id);

    // Confirm by Charlie
    start_cheat_caller_address(contract_address, CHARLIE);
    state.confirm_transaction(id);

    assert_tx_state(id, TransactionState::Confirmed);
    state._change_quorum(quorum + 1);
    assert_tx_state(id, TransactionState::Pending);
}

#[test]
fn test_pending_status_changed_when_quorum_reduced() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let contract_address = test_address();
    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));

    // Submit by Alice
    start_cheat_caller_address(contract_address, ALICE);
    let id = state.submit_transaction(to, selector, calldata, 0);

    // Confirm by Bob
    start_cheat_caller_address(contract_address, BOB);
    state.confirm_transaction(id);

    assert_tx_state(id, TransactionState::Pending);
    state._change_quorum(quorum - 1);
    assert_tx_state(id, TransactionState::Confirmed);
}

#[test]
fn test_confirm_tx_batch() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let mut spy = spy_events();
    let contract_address = test_address();

    let calls = array![
        build_call(MockCall::AddNumber(42)),
        build_call(MockCall::AddNumber(18)),
        build_call(MockCall::AddNumber(40)),
    ]
        .span();

    // Submit by Alice
    start_cheat_caller_address(contract_address, ALICE);
    let id = state.submit_transaction_batch(calls, 0);
    assert_tx_state(id, TransactionState::Pending);
    assert_eq!(state.get_transaction_confirmations(id), 0);
    spy.drop_all_events();

    // Confirm by Bob
    start_cheat_caller_address(contract_address, BOB);
    assert_eq!(state.is_confirmed_by(id, BOB), false);
    state.confirm_transaction(id);
    assert_eq!(state.is_confirmed_by(id, BOB), true);
    assert_tx_state(id, TransactionState::Pending);
    assert_eq!(state.get_transaction_confirmations(id), 1);
    spy.assert_only_event_tx_confirmed(contract_address, id, BOB);

    // Confirm by Charlie
    start_cheat_caller_address(contract_address, CHARLIE);
    assert_eq!(state.is_confirmed_by(id, CHARLIE), false);
    state.confirm_transaction(id);
    assert_eq!(state.is_confirmed_by(id, CHARLIE), true);
    assert_tx_state(id, TransactionState::Confirmed);
    assert_eq!(state.get_transaction_confirmations(id), 2);
    spy.assert_only_event_tx_confirmed(contract_address, id, CHARLIE);
}

#[test]
#[should_panic(expected: 'Multisig: tx not found')]
fn test_cannot_confirm_nonexistent_tx() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let contract_address = test_address();

    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    let id = state.hash_transaction(to, selector, calldata, 0);

    start_cheat_caller_address(contract_address, ALICE);
    state.confirm_transaction(id);
}

#[test]
#[should_panic(expected: 'Multisig: not a signer')]
fn test_cannot_confirm_tx_unauthorized() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let contract_address = test_address();

    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    let id = state.hash_transaction(to, selector, calldata, 0);
    start_cheat_caller_address(contract_address, ALICE);
    state.submit_transaction(to, selector, calldata, 0);

    start_cheat_caller_address(contract_address, OTHER);
    state.confirm_transaction(id);
}

#[test]
#[should_panic(expected: 'Multisig: already confirmed')]
fn test_cannot_confirm_tx_twice() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let contract_address = test_address();

    // Submit by Alice
    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    let id = state.hash_transaction(to, selector, calldata, 0);
    start_cheat_caller_address(contract_address, ALICE);
    state.submit_transaction(to, selector, calldata, 0);

    // Confirm by Bob
    start_cheat_caller_address(contract_address, BOB);
    state.confirm_transaction(id);
    assert_eq!(state.is_confirmed_by(id, BOB), true);
    assert_eq!(state.get_transaction_confirmations(id), 1);

    // Try to confirm again by Bob
    state.confirm_transaction(id);
}

//
// Revoke confirmation
//

#[test]
fn test_revoke_confirmation() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let mut spy = spy_events();
    let contract_address = test_address();

    // Submit by Alice
    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    start_cheat_caller_address(contract_address, ALICE);
    let id = state.submit_transaction(to, selector, calldata, 0);

    // Confirm by Bob
    start_cheat_caller_address(contract_address, BOB);
    state.confirm_transaction(id);

    // Confirm by Charlie
    start_cheat_caller_address(contract_address, CHARLIE);
    state.confirm_transaction(id);

    // Revoke confirmation by Charlie
    spy.drop_all_events();
    assert_tx_state(id, TransactionState::Confirmed);
    assert_eq!(state.is_confirmed_by(id, CHARLIE), true);
    assert_eq!(state.get_transaction_confirmations(id), 2);
    state.revoke_confirmation(id);
    assert_tx_state(id, TransactionState::Pending);
    assert_eq!(state.is_confirmed_by(id, CHARLIE), false);
    assert_eq!(state.get_transaction_confirmations(id), 1);
    spy.assert_only_event_confirmation_revoked(contract_address, id, CHARLIE);
}

#[test]
fn test_tx_not_confirmed_after_signer_removal() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let contract_address = test_address();

    // Submit & confirm by Alice
    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    start_cheat_caller_address(contract_address, ALICE);
    let id = state.submit_transaction(to, selector, calldata, 0);
    state.confirm_transaction(id);

    // Confirm by Bob
    start_cheat_caller_address(contract_address, BOB);
    state.confirm_transaction(id);

    // Check state before removal
    assert_tx_state(id, TransactionState::Confirmed);
    assert_eq!(state.is_confirmed_by(id, BOB), true);
    assert_eq!(state.get_transaction_confirmations(id), 2);

    // Remove Bob from signers
    start_cheat_caller_address(contract_address, contract_address);
    state.remove_signers(quorum, array![BOB].span());

    // Check state after removal
    assert_tx_state(id, TransactionState::Pending);
    assert_eq!(state.is_confirmed_by(id, BOB), true);
    assert_eq!(state.get_transaction_confirmations(id), 1);
}

#[test]
fn test_can_revoke_confirmation_after_being_removed() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let contract_address = test_address();

    // Submit & confirm by Alice
    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    start_cheat_caller_address(contract_address, ALICE);
    let id = state.submit_transaction(to, selector, calldata, 0);
    state.confirm_transaction(id);

    // Confirm by Bob
    start_cheat_caller_address(contract_address, BOB);
    state.confirm_transaction(id);

    // Remove Bob from signers
    start_cheat_caller_address(contract_address, contract_address);
    state.remove_signers(quorum, array![BOB].span());

    // Check state before revocation
    assert_tx_state(id, TransactionState::Pending);
    assert_eq!(state.is_confirmed_by(id, BOB), true);
    assert_eq!(state.get_transaction_confirmations(id), 1);

    // Revoke confirmation by Bob
    start_cheat_caller_address(contract_address, BOB);
    state.revoke_confirmation(id);

    // Check state after revocation
    assert_tx_state(id, TransactionState::Pending);
    assert_eq!(state.is_confirmed_by(id, BOB), false);
    assert_eq!(state.get_transaction_confirmations(id), 1);
}

#[test]
#[should_panic(expected: 'Multisig: has not confirmed')]
fn test_cannot_revoke_confirmation_has_not_confirmed() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);

    // Submit by Alice
    start_cheat_caller_address(test_address(), ALICE);
    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    let id = state.submit_transaction(to, selector, calldata, 0);

    // Revoke confirmation by Bob
    start_cheat_caller_address(test_address(), BOB);
    state.revoke_confirmation(id);
}

#[test]
#[should_panic(expected: 'Multisig: tx not found')]
fn test_cannot_revoke_confirmation_nonexistent_tx() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);

    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    let id = state.hash_transaction(to, selector, calldata, 0);
    state.revoke_confirmation(id);
}

//
// Execute tx
//

#[test]
fn test_execute_tx() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let mut spy = spy_events();
    let mock = deploy_mock();
    let contract_address = test_address();

    // Submit
    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    let salt = 0;
    start_cheat_caller_address(contract_address, ALICE);
    let id = state.submit_transaction(to, selector, calldata, salt);

    // Confirm
    start_cheat_caller_address(contract_address, BOB);
    state.confirm_transaction(id);
    start_cheat_caller_address(contract_address, CHARLIE);
    state.confirm_transaction(id);

    // Check state before
    assert_eq!(mock.get_current_sum(), 0);
    assert_tx_state(id, TransactionState::Confirmed);

    // Execute
    spy.drop_all_events();
    start_cheat_caller_address(contract_address, ALICE);
    state.execute_transaction(to, selector, calldata, salt);

    // Check state after
    assert_eq!(mock.get_current_sum(), 42);
    assert_tx_state(id, TransactionState::Executed);
    spy.assert_only_event_tx_executed(contract_address, id);
}

#[test]
fn test_execute_tx_batch() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let contract_address = test_address();
    let mut spy = spy_events();
    let mock = deploy_mock();
    let calls = array![
        build_call(MockCall::AddNumber(42)),
        build_call(MockCall::AddNumber(18)),
        build_call(MockCall::AddNumber(40)),
    ]
        .span();
    let salt = 0;

    // Submit
    start_cheat_caller_address(contract_address, ALICE);
    let id = state.submit_transaction_batch(calls, salt);

    // Confirm
    start_cheat_caller_address(contract_address, BOB);
    state.confirm_transaction(id);
    start_cheat_caller_address(contract_address, CHARLIE);
    state.confirm_transaction(id);

    // Check state before
    assert_eq!(mock.get_current_sum(), 0);
    assert_tx_state(id, TransactionState::Confirmed);

    // Execute
    spy.drop_all_events();
    start_cheat_caller_address(contract_address, ALICE);
    state.execute_transaction_batch(calls, salt);

    // Check state after
    assert_eq!(mock.get_current_sum(), 100);
    assert_tx_state(id, TransactionState::Executed);
    spy.assert_only_event_tx_executed(contract_address, id);
}

#[test]
#[should_panic(expected: 'Multisig: tx not found')]
fn test_cannot_execute_not_submitted_tx() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let contract_address = test_address();
    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    let salt = 0;

    // Try to execute
    start_cheat_caller_address(contract_address, ALICE);
    state.execute_transaction(to, selector, calldata, salt);
}

#[test]
#[should_panic(expected: 'Multisig: not a signer')]
fn test_cannot_execute_unauthorized() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let contract_address = test_address();
    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    let salt = 0;

    // Submit
    start_cheat_caller_address(contract_address, ALICE);
    let id = state.submit_transaction(to, selector, calldata, salt);

    // Confirm
    start_cheat_caller_address(contract_address, BOB);
    state.confirm_transaction(id);
    start_cheat_caller_address(contract_address, CHARLIE);
    state.confirm_transaction(id);

    // Try to execute
    start_cheat_caller_address(contract_address, OTHER);
    state.execute_transaction(to, selector, calldata, salt);
}

#[test]
#[should_panic(expected: 'Multisig: not a signer')]
fn test_cannot_execute_batch_unauthorized() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let contract_address = test_address();
    let calls = array![
        build_call(MockCall::AddNumber(42)),
        build_call(MockCall::AddNumber(18)),
        build_call(MockCall::AddNumber(40)),
    ]
        .span();
    let salt = 0;

    // Submit
    start_cheat_caller_address(contract_address, ALICE);
    let id = state.submit_transaction_batch(calls, salt);

    // Confirm
    start_cheat_caller_address(contract_address, BOB);
    state.confirm_transaction(id);
    start_cheat_caller_address(contract_address, CHARLIE);
    state.confirm_transaction(id);

    // Try to execute
    start_cheat_caller_address(contract_address, OTHER);
    state.execute_transaction_batch(calls, salt);
}

#[test]
#[should_panic(expected: 'Multisig: tx not confirmed')]
fn test_cannot_execute_not_confirmed() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let contract_address = test_address();
    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    let salt = 0;

    // Submit
    start_cheat_caller_address(contract_address, ALICE);
    let id = state.submit_transaction(to, selector, calldata, salt);

    // Confirm once
    start_cheat_caller_address(contract_address, BOB);
    state.confirm_transaction(id);

    // Execute
    state.execute_transaction(to, selector, calldata, salt);
}

#[test]
#[should_panic(expected: 'Multisig: tx not confirmed')]
fn test_cannot_execute_batch_not_confirmed() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let contract_address = test_address();
    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    let salt = 0;

    // Submit
    start_cheat_caller_address(contract_address, ALICE);
    let id = state.submit_transaction(to, selector, calldata, salt);

    // Confirm once
    start_cheat_caller_address(contract_address, BOB);
    state.confirm_transaction(id);

    // Execute
    state.execute_transaction(to, selector, calldata, salt);
}

#[test]
#[should_panic(expected: 'Multisig: tx already executed')]
fn test_cannot_execute_twice() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let contract_address = test_address();
    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    let salt = 0;
    deploy_mock();

    // Submit
    start_cheat_caller_address(contract_address, ALICE);
    let id = state.submit_transaction(to, selector, calldata, salt);

    // Confirm
    start_cheat_caller_address(contract_address, BOB);
    state.confirm_transaction(id);
    start_cheat_caller_address(contract_address, CHARLIE);
    state.confirm_transaction(id);

    // Execute 1st time
    state.execute_transaction(to, selector, calldata, salt);

    // Try to execute 2nd time
    state.execute_transaction(to, selector, calldata, salt);
}

#[test]
#[should_panic(expected: 'Multisig: tx already executed')]
fn test_cannot_execute_batch_twice() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let contract_address = test_address();
    deploy_mock();

    // Submit
    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    let salt = 0;
    start_cheat_caller_address(contract_address, ALICE);
    let id = state.submit_transaction(to, selector, calldata, salt);

    // Confirm
    start_cheat_caller_address(contract_address, BOB);
    state.confirm_transaction(id);
    start_cheat_caller_address(contract_address, CHARLIE);
    state.confirm_transaction(id);

    // Execute 1st time
    state.execute_transaction(to, selector, calldata, salt);

    // Try to execute 2nd time
    state.execute_transaction(to, selector, calldata, salt);
}

//
// hash_transaction
//

#[test]
fn test_tx_hash_depends_on_salt() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let Call { to, selector, calldata } = build_call(MockCall::AddNumber(42));
    start_cheat_caller_address(test_address(), ALICE);

    let mut salt = 0;
    while salt != 10 {
        let id_from_hash = state.hash_transaction(to, selector, calldata, salt);
        let id = state.submit_transaction(to, selector, calldata, salt);
        assert_eq!(id_from_hash, id);
        salt += 1;
    };
}

#[test]
fn test_tx_batch_hash_depends_on_salt() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let calls = array![
        build_call(MockCall::AddNumber(42)),
        build_call(MockCall::AddNumber(18)),
        build_call(MockCall::AddNumber(40)),
    ]
        .span();
    start_cheat_caller_address(test_address(), ALICE);

    let mut salt = 0;
    while salt != 10 {
        let id_from_hash = state.hash_transaction_batch(calls, salt);
        let id = state.submit_transaction_batch(calls, salt);
        assert_eq!(id_from_hash, id);
        salt += 1;
    };
}

#[test]
fn test_tx_hash_depends_on_calldata() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    start_cheat_caller_address(test_address(), ALICE);

    let mut num = 0;
    while num != 10 {
        let Call { to, selector, calldata } = build_call(MockCall::AddNumber(num));
        let id_from_hash = state.hash_transaction(to, selector, calldata, SALT);
        let id = state.submit_transaction(to, selector, calldata, SALT);
        assert_eq!(id_from_hash, id);
        num += 1;
    };
}

#[test]
fn test_tx_hash_depends_on_selector() {
    let (quorum, signers) = DEFAULT_DATA();
    let state = setup_component(quorum, signers);

    let to = MOCK_ADDRESS;
    let empty_calldata = array![].span();
    let id_1 = state.hash_transaction(to, selector!("selector_1"), empty_calldata, SALT);
    let id_2 = state.hash_transaction(to, selector!("selector_2"), empty_calldata, SALT);
    let id_3 = state.hash_transaction(to, selector!("selector_3"), empty_calldata, SALT);
    assert!(id_1 != id_2);
    assert!(id_2 != id_3);
    assert!(id_1 != id_3);
}

#[test]
fn test_tx_hash_depends_on_to_address() {
    let (quorum, signers) = DEFAULT_DATA();
    let state = setup_component(quorum, signers);

    let Call { to: _, selector, calldata } = build_call(MockCall::AddNumber(42));
    let id_1 = state.hash_transaction(ALICE, selector, calldata, SALT);
    let id_2 = state.hash_transaction(BOB, selector, calldata, SALT);
    let id_3 = state.hash_transaction(CHARLIE, selector, calldata, SALT);
    assert!(id_1 != id_2);
    assert!(id_2 != id_3);
    assert!(id_1 != id_3);
}

//
// add_signers
//

#[test]
fn test_add_single_signer() {
    let quorum = 1;
    let (alice, bob, charlie) = (ALICE, BOB, CHARLIE);
    let mut state = setup_component(quorum, array![alice].span());
    let contract_address = test_address();
    let mut spy = spy_events();
    start_cheat_caller_address(contract_address, contract_address);

    // Add Bob as signer
    state.add_signers(quorum, array![bob].span());
    assert_signers_list(array![alice, bob].span());
    spy.assert_only_event_signer_added(contract_address, bob);

    // Add Charlie as signer
    state.add_signers(quorum, array![charlie].span());
    assert_signers_list(array![alice, bob, charlie].span());
    spy.assert_only_event_signer_added(contract_address, charlie);
}

#[test]
fn test_add_multiple_signers() {
    let quorum = 1;
    let (alice, bob, charlie) = (ALICE, BOB, CHARLIE);
    let mut state = setup_component(quorum, array![alice].span());
    let contract_address = test_address();
    let mut spy = spy_events();
    start_cheat_caller_address(contract_address, contract_address);

    // Add Bob and Charlie as signers
    state.add_signers(quorum, array![bob, charlie].span());
    assert_signers_list(array![alice, bob, charlie].span());
    spy.assert_event_signer_added(contract_address, bob);
    spy.assert_only_event_signer_added(contract_address, charlie);
}

#[test]
fn test_add_remove_add() {
    let quorum = 1;
    let (alice, bob, charlie) = (ALICE, BOB, CHARLIE);
    let mut state = setup_component(quorum, array![alice].span());
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, contract_address);

    // Add Bob and Charlie as signers
    state.add_signers(quorum, array![bob, charlie].span());
    assert_signers_list(array![alice, bob, charlie].span());

    // Remove Alice
    state.remove_signers(quorum, array![alice].span());
    assert_signers_list(array![charlie, bob].span());

    // Remove Charlie
    state.remove_signers(quorum, array![charlie].span());
    assert_signers_list(array![bob].span());

    // Add Alice
    state.add_signers(quorum, array![alice].span());
    assert_signers_list(array![bob, alice].span());

    // Add Charlie
    state.add_signers(quorum, array![charlie].span());
    assert_signers_list(array![bob, alice, charlie].span());
}

#[test]
fn test_signers_ignored_if_added_again() {
    let quorum = 1;
    let (alice, bob, charlie) = (ALICE, BOB, CHARLIE);
    let mut state = setup_component(quorum, array![alice].span());
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, contract_address);

    // Add Bob as signer
    state.add_signers(quorum, array![bob].span());
    assert_signers_list(array![alice, bob].span());

    // Add Alice and Bob again and Charlie as a new signer
    let mut spy = spy_events();
    state.add_signers(quorum, array![alice, bob, charlie].span());
    assert_signers_list(array![alice, bob, charlie].span());
    spy.assert_only_event_signer_added(contract_address, charlie);
}

#[test]
fn test_add_signers_does_nothing_if_signers_empty() {
    let (quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(quorum, signers);
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, contract_address);

    // Call `add_signers` with an empty list
    let mut spy = spy_events();
    let empty_list = array![].span();
    state.add_signers(quorum, empty_list);
    spy.assert_no_events_left_from(contract_address);
}

#[test]
#[should_panic(expected: 'Multisig: unauthorized')]
fn test_cannot_add_when_not_multisig_itself() {
    let quorum = 1;
    let (alice, bob) = (ALICE, BOB);
    let mut state = setup_component(quorum, array![alice].span());

    // Try to add signer
    start_cheat_caller_address(test_address(), OTHER);
    state.add_signers(quorum, array![bob].span());
}

#[test]
#[should_panic(expected: 'Multisig: zero address signer')]
fn test_cannot_add_zero_address_as_signer() {
    let quorum = 1;
    let mut state = setup_component(quorum, array![ALICE].span());
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, contract_address);

    // Try to add zero address as signer
    state.add_signers(quorum, array![ZERO].span());
}

#[test]
#[should_panic(expected: 'Multisig: quorum cannot be 0')]
fn test_cannot_add_with_zero_quorum() {
    let quorum = 1;
    let (alice, bob) = (ALICE, BOB);
    let mut state = setup_component(quorum, array![alice].span());
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, contract_address);

    // Try to add with 0 quorum value
    state.add_signers(0, array![bob].span());
}

#[test]
#[should_panic(expected: 'Multisig: quorum > signers')]
fn test_cannot_add_with_quorum_too_high() {
    let quorum = 1;
    let (alice, bob, charlie) = (ALICE, BOB, CHARLIE);
    let mut state = setup_component(quorum, array![alice].span());
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, contract_address);

    // Try to add with quorum value > signers count
    state.add_signers(4, array![bob, charlie].span());
}

//
// remove_signers
//

#[test]
fn test_remove_single_signer() {
    let quorum = 1;
    let (alice, bob, charlie) = (ALICE, BOB, CHARLIE);
    let mut state = setup_component(quorum, array![alice, bob, charlie].span());
    let contract_address = test_address();
    let mut spy = spy_events();
    start_cheat_caller_address(contract_address, contract_address);

    // Remove Alice from signers
    assert_signers_list(array![alice, bob, charlie].span());
    state.remove_signers(quorum, array![alice].span());
    assert_signers_list(array![charlie, bob].span());
    spy.assert_only_event_signer_removed(contract_address, alice);
    assert_eq!(state.is_signer(alice), false);

    // Remove Charlie from signers
    state.remove_signers(quorum, array![charlie].span());
    assert_signers_list(array![bob].span());
    spy.assert_only_event_signer_removed(contract_address, charlie);
    assert_eq!(state.is_signer(charlie), false);
}

#[test]
fn test_remove_multiple_signers() {
    let quorum = 1;
    let (alice, bob, charlie, other) = (ALICE, BOB, CHARLIE, OTHER);
    let mut state = setup_component(quorum, array![alice, bob, charlie, other].span());
    let contract_address = test_address();
    let mut spy = spy_events();
    start_cheat_caller_address(contract_address, contract_address);

    // Remove Alice and Other from signers
    assert_signers_list(array![alice, bob, charlie, other].span());
    state.remove_signers(quorum, array![alice, other].span());
    assert_signers_list(array![charlie, bob].span());
    spy.assert_event_signer_removed(contract_address, alice);
    spy.assert_only_event_signer_removed(contract_address, other);
    assert_eq!(state.is_signer(alice), false);
    assert_eq!(state.is_signer(other), false);
}

#[test]
fn test_remove_add_remove() {
    let quorum = 1;
    let (alice, bob, charlie) = (ALICE, BOB, CHARLIE);
    let mut state = setup_component(quorum, array![alice, bob, charlie].span());
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, contract_address);

    // Remove Alice from signers
    state.remove_signers(quorum, array![alice].span());
    assert_signers_list(array![charlie, bob].span());
    assert_eq!(state.is_signer(alice), false);

    // Add Alice to signers
    state.add_signers(quorum, array![alice].span());
    assert_signers_list(array![charlie, bob, alice].span());
    assert_eq!(state.is_signer(alice), true);

    // Remove Alice from signers
    state.remove_signers(quorum, array![alice].span());
    assert_signers_list(array![charlie, bob].span());
    assert_eq!(state.is_signer(alice), false);
}

#[test]
fn test_not_signers_ignored_when_removing() {
    let quorum = 1;
    let (alice, bob, charlie, other) = (ALICE, BOB, CHARLIE, OTHER);
    let mut state = setup_component(quorum, array![alice, bob, charlie].span());
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, contract_address);

    // Remove Alice and Other from signers
    let mut spy = spy_events();
    state.remove_signers(quorum, array![alice, other].span());
    assert_signers_list(array![charlie, bob].span());
    assert_eq!(state.is_signer(alice), false);
    assert_eq!(state.is_signer(other), false);
    spy.assert_only_event_signer_removed(contract_address, alice);
}

#[test]
fn test_remove_signers_does_nothing_if_signers_empty() {
    let quorum = 1;
    let (alice, bob, charlie) = (ALICE, BOB, CHARLIE);
    let mut state = setup_component(quorum, array![alice, bob, charlie].span());
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, contract_address);

    // Call `remove_signers` with an empty list
    let mut spy = spy_events();
    let empty_list = array![].span();
    state.remove_signers(quorum, empty_list);
    assert_signers_list(array![alice, bob, charlie].span());
    spy.assert_no_events_left_from(contract_address);
}

#[test]
#[should_panic(expected: 'Multisig: unauthorized')]
fn test_cannot_remove_when_not_multisig_itself() {
    let quorum = 1;
    let (alice, bob, charlie) = (ALICE, BOB, CHARLIE);
    let mut state = setup_component(quorum, array![alice, bob, charlie].span());

    // Try to call 'remove_signers' from Other
    start_cheat_caller_address(test_address(), OTHER);
    state.remove_signers(quorum, array![alice].span());
}

#[test]
#[should_panic(expected: 'Multisig: quorum cannot be 0')]
fn test_cannot_remove_with_zero_quorum() {
    let quorum = 1;
    let (alice, bob, charlie) = (ALICE, BOB, CHARLIE);
    let mut state = setup_component(quorum, array![alice, bob, charlie].span());
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, contract_address);

    // Try to remove signers with 0 quorum value
    state.remove_signers(0, array![bob, charlie].span());
}

#[test]
#[should_panic(expected: 'Multisig: quorum > signers')]
fn test_cannot_remove_with_quorum_too_high() {
    let quorum = 1;
    let (alice, bob, charlie) = (ALICE, BOB, CHARLIE);
    let mut state = setup_component(quorum, array![alice, bob, charlie].span());
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, contract_address);

    // Try to remove signers with quorum value too high
    state.remove_signers(3, array![bob].span());
}

#[test]
#[should_panic(expected: 'Multisig: quorum > signers')]
fn test_cannot_remove_with_unchanged_quorum_that_becomes_too_high() {
    let quorum = 4;
    let (alice, bob, charlie, other) = (ALICE, BOB, CHARLIE, OTHER);
    let mut state = setup_component(quorum, array![alice, bob, charlie, other].span());
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, contract_address);

    // Shouldn't be allowed. Quorum value of 4 with only 2 signers left
    // will lead to the contract becoming permanently inaccessible.
    state.remove_signers(quorum, array![alice, other].span());
}

//
// replace_signer
//

#[test]
fn test_replace_signer() {
    let quorum = 1;
    let (alice, bob, charlie) = (ALICE, BOB, CHARLIE);
    let mut state = setup_component(quorum, array![alice, bob].span());
    let contract_address = test_address();
    let mut spy = spy_events();
    start_cheat_caller_address(contract_address, contract_address);

    // Check state before
    assert_signers_list(array![alice, bob].span());
    assert_eq!(state.is_signer(alice), true);
    assert_eq!(state.is_signer(charlie), false);

    // Replace Alice with Charlie
    state.replace_signer(alice, charlie);
    spy.assert_event_signer_removed(contract_address, alice);
    spy.assert_only_event_signer_added(contract_address, charlie);

    // Check state after
    assert_signers_list(array![charlie, bob].span());
    assert_eq!(state.is_signer(alice), false);
    assert_eq!(state.is_signer(charlie), true);
}

#[test]
#[should_panic(expected: 'Multisig: not a signer')]
fn test_cannot_replace_not_signer() {
    let quorum = 1;
    let (alice, bob, charlie) = (ALICE, BOB, CHARLIE);
    let mut state = setup_component(quorum, array![alice, bob].span());
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, contract_address);

    // Try to replace not a signer
    state.replace_signer(OTHER, charlie);
}

#[test]
#[should_panic(expected: 'Multisig: already a signer')]
fn test_cannot_replace_with_existing_signer() {
    let quorum = 1;
    let (alice, bob) = (ALICE, BOB);
    let mut state = setup_component(quorum, array![alice, bob].span());
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, contract_address);

    // Try to replace with existing signer
    state.replace_signer(alice, bob);
}

#[test]
#[should_panic(expected: 'Multisig: zero address signer')]
fn test_cannot_replace_with_zero_address() {
    let quorum = 1;
    let (alice, bob, charlie) = (ALICE, BOB, CHARLIE);
    let mut state = setup_component(quorum, array![alice, bob, charlie].span());
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, contract_address);

    // Try to replace with zero address
    state.replace_signer(alice, ZERO);
}

//
// change_quorum
//

#[test]
fn test_change_quorum_higher_value() {
    let (initial_quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(initial_quorum, signers);
    let contract_address = test_address();
    let mut spy = spy_events();
    start_cheat_caller_address(contract_address, contract_address);

    // Increase quorum value
    let new_quorum = initial_quorum + 1;
    state.change_quorum(new_quorum);
    assert_eq!(state.get_quorum(), new_quorum);
    spy.assert_only_event_quorum_updated(contract_address, initial_quorum, new_quorum);
}

#[test]
fn test_change_quorum_lower_value() {
    let (initial_quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(initial_quorum, signers);
    let contract_address = test_address();
    let mut spy = spy_events();
    start_cheat_caller_address(contract_address, contract_address);

    // Reduce quorum value
    let new_quorum = initial_quorum - 1;
    state.change_quorum(new_quorum);
    assert_eq!(state.get_quorum(), new_quorum);
    spy.assert_only_event_quorum_updated(contract_address, initial_quorum, new_quorum);
}

#[test]
fn test_change_quorum_to_same_value() {
    let (initial_quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(initial_quorum, signers);
    let contract_address = test_address();
    let mut spy = spy_events();
    start_cheat_caller_address(contract_address, contract_address);

    // Call 'change_quorum' with the current quorum value
    state.change_quorum(initial_quorum);
    assert_eq!(state.get_quorum(), initial_quorum);
    spy.assert_no_events_left_from(contract_address);
}

#[test]
fn test_change_quorum_to_min_value() {
    let (initial_quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(initial_quorum, signers);
    let contract_address = test_address();
    let mut spy = spy_events();
    start_cheat_caller_address(contract_address, contract_address);

    // Set quorum to min allowed value (1)
    let new_quorum = 1;
    state.change_quorum(new_quorum);
    assert_eq!(state.get_quorum(), new_quorum);
    spy.assert_only_event_quorum_updated(contract_address, initial_quorum, new_quorum);
}

#[test]
fn test_change_quorum_to_max_value() {
    let (initial_quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(initial_quorum, signers);
    let contract_address = test_address();
    let mut spy = spy_events();
    start_cheat_caller_address(contract_address, contract_address);

    // Set quorum to max allowed value (signers count)
    let new_quorum = signers.len();
    state.change_quorum(new_quorum);
    assert_eq!(state.get_quorum(), new_quorum);
    spy.assert_only_event_quorum_updated(contract_address, initial_quorum, new_quorum);
}

#[test]
#[should_panic(expected: 'Multisig: quorum cannot be 0')]
fn test_cannot_change_quorum_to_zero() {
    let (initial_quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(initial_quorum, signers);
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, contract_address);

    // Try to set quorum to 0
    state.change_quorum(0);
}

#[test]
#[should_panic(expected: 'Multisig: quorum > signers')]
fn test_cannot_set_quorum_too_high() {
    let (initial_quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(initial_quorum, signers);
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, contract_address);

    // Try to set quorum to a value too high (signers count + 1)
    let new_quorum = signers.len() + 1;
    state.change_quorum(new_quorum);
}

#[test]
#[should_panic(expected: 'Multisig: unauthorized')]
fn test_cannot_change_quorum_when_not_multisig_itself() {
    let (initial_quorum, signers) = DEFAULT_DATA();
    let mut state = setup_component(initial_quorum, signers);
    start_cheat_caller_address(test_address(), OTHER);

    // Try to set quorum to 0
    state.change_quorum(0);
}

#[test]
fn test_signers_info_error_happens_with_v1() {
    let quorum = 123;
    let signers_count = Bounded::MAX;
    let info = SignersInfo { quorum, signers_count };
    let packed_value = LegacySignersInfoStorePackingV1::pack(info);
    let unpacked_info = LegacySignersInfoStorePackingV1::unpack(packed_value);

    assert_eq!(unpacked_info.quorum, quorum + 1);
    assert_eq!(unpacked_info.signers_count, 0);
}

#[test]
fn test_signers_info_no_error_happens_with_v2() {
    let quorum = 123;
    let signers_count = Bounded::MAX;
    let info = SignersInfo { quorum, signers_count };
    let packed_value = SignersInfoStorePackingV2::pack(info);
    let unpacked_info = SignersInfoStorePackingV2::unpack(packed_value);

    assert_eq!(unpacked_info.quorum, quorum);
    assert_eq!(unpacked_info.signers_count, signers_count);
}

#[test]
fn test_signers_info_pack_unpack_v2_max_values() {
    let quorum = Bounded::MAX;
    let signers_count = Bounded::MAX;
    let info = SignersInfo { quorum, signers_count };
    let packed_value = SignersInfoStorePackingV2::pack(info);
    let unpacked_info = SignersInfoStorePackingV2::unpack(packed_value);

    assert_eq!(unpacked_info.quorum, quorum);
    assert_eq!(unpacked_info.signers_count, signers_count);
}

#[test]
fn test_signers_info_unpack_zero_value_v2() {
    let packed_value = 0;
    let unpacked_info = SignersInfoStorePackingV2::unpack(packed_value);

    assert_eq!(unpacked_info.quorum, 0);
    assert_eq!(unpacked_info.signers_count, 0);
}

//
// Helpers
//

#[derive(Copy, Drop)]
enum MockCall {
    AddNumber: felt252,
    FailingFn,
    BadSelector,
}

fn build_call(call: MockCall) -> Call {
    let (selector, calldata) = match call {
        MockCall::AddNumber(number) => (selector!("add_number"), array![number]),
        MockCall::FailingFn => (selector!("failing_function"), array![]),
        MockCall::BadSelector => (selector!("bad_selector"), array![]),
    };
    Call { to: MOCK_ADDRESS, selector, calldata: calldata.span() }
}

fn assert_tx_state(id: TransactionID, expected_state: TransactionState) {
    let state = COMPONENT_STATE();
    let tx_state = state.get_transaction_state(id);
    let block = state.get_submitted_block(id);
    let is_confirmed = state.is_confirmed(id);
    let is_executed = state.is_executed(id);
    let tx_confirmations = state.get_transaction_confirmations(id);

    assert_eq!(tx_state, expected_state);
    match expected_state {
        TransactionState::NotFound => {
            assert!(block.is_zero());
            assert!(!is_confirmed);
            assert!(!is_executed);
            assert!(tx_confirmations.is_zero());
        },
        TransactionState::Pending => {
            assert!(block.is_non_zero());
            assert!(!is_confirmed);
            assert!(!is_executed);
            assert!(tx_confirmations < state.get_quorum());
        },
        TransactionState::Confirmed => {
            assert!(block.is_non_zero());
            assert!(is_confirmed);
            assert!(!is_executed);
            assert!(tx_confirmations >= state.get_quorum());
        },
        TransactionState::Executed => {
            assert!(block.is_non_zero());
            assert!(is_confirmed);
            assert!(is_executed);
            assert!(tx_confirmations >= state.get_quorum());
        },
    };
}

fn assert_signers_list(expected_signers: Span<ContractAddress>) {
    let state = COMPONENT_STATE();
    let actual_signers = state.get_signers();
    assert_eq!(actual_signers, expected_signers);
    for signer in expected_signers {
        assert!(state.is_signer(*signer));
    };
}

const MAX_U32: NonZero<u128> = 0xffffffff;

impl LegacySignersInfoStorePackingV1 of StorePacking<SignersInfo, u128> {
    fn pack(value: SignersInfo) -> u128 {
        let SignersInfo { quorum, signers_count } = value;
        quorum.into() * MAX_U32.into() + signers_count.into()
    }

    fn unpack(value: u128) -> SignersInfo {
        let (quorum, signers_count) = u128_safe_divmod(value, MAX_U32);
        SignersInfo {
            quorum: quorum.try_into().unwrap(), signers_count: signers_count.try_into().unwrap(),
        }
    }
}

//
// Events
//

#[generate_trait]
impl MultisigSpyHelpersImpl of MultisigSpyHelpers {
    //
    // SignerAdded
    //

    fn assert_event_signer_added(
        ref self: EventSpy, contract: ContractAddress, signer: ContractAddress,
    ) {
        let expected = Event::SignerAdded(SignerAdded { signer });
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_signer_added(
        ref self: EventSpy, contract: ContractAddress, signer: ContractAddress,
    ) {
        self.assert_event_signer_added(contract, signer);
        self.assert_no_events_left_from(contract);
    }

    //
    // SignerRemoved
    //

    fn assert_event_signer_removed(
        ref self: EventSpy, contract: ContractAddress, signer: ContractAddress,
    ) {
        let expected = Event::SignerRemoved(SignerRemoved { signer });
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_signer_removed(
        ref self: EventSpy, contract: ContractAddress, signer: ContractAddress,
    ) {
        self.assert_event_signer_removed(contract, signer);
        self.assert_no_events_left_from(contract);
    }

    //
    // QuorumUpdated
    //

    fn assert_event_quorum_updated(
        ref self: EventSpy, contract: ContractAddress, old_quorum: u32, new_quorum: u32,
    ) {
        let expected = Event::QuorumUpdated(QuorumUpdated { old_quorum, new_quorum });
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_quorum_updated(
        ref self: EventSpy, contract: ContractAddress, old_quorum: u32, new_quorum: u32,
    ) {
        self.assert_event_quorum_updated(contract, old_quorum, new_quorum);
        self.assert_no_events_left_from(contract);
    }

    //
    // TransactionSubmitted
    //

    fn assert_event_tx_submitted(
        ref self: EventSpy, contract: ContractAddress, id: TransactionID, signer: ContractAddress,
    ) {
        let expected = Event::TransactionSubmitted(TransactionSubmitted { id, signer });
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_tx_submitted(
        ref self: EventSpy, contract: ContractAddress, id: TransactionID, signer: ContractAddress,
    ) {
        self.assert_event_tx_submitted(contract, id, signer);
        self.assert_no_events_left_from(contract);
    }

    //
    // TransactionConfirmed
    //

    fn assert_event_tx_confirmed(
        ref self: EventSpy, contract: ContractAddress, id: TransactionID, signer: ContractAddress,
    ) {
        let expected = Event::TransactionConfirmed(TransactionConfirmed { id, signer });
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_tx_confirmed(
        ref self: EventSpy, contract: ContractAddress, id: TransactionID, signer: ContractAddress,
    ) {
        self.assert_event_tx_confirmed(contract, id, signer);
        self.assert_no_events_left_from(contract);
    }

    //
    // ConfirmationRevoked
    //

    fn assert_event_confirmation_revoked(
        ref self: EventSpy, contract: ContractAddress, id: TransactionID, signer: ContractAddress,
    ) {
        let expected = Event::ConfirmationRevoked(ConfirmationRevoked { id, signer });
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_confirmation_revoked(
        ref self: EventSpy, contract: ContractAddress, id: TransactionID, signer: ContractAddress,
    ) {
        self.assert_event_confirmation_revoked(contract, id, signer);
        self.assert_no_events_left_from(contract);
    }

    //
    // TransactionExecuted
    //

    fn assert_event_tx_executed(ref self: EventSpy, contract: ContractAddress, id: TransactionID) {
        let expected = Event::TransactionExecuted(TransactionExecuted { id });
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_tx_executed(
        ref self: EventSpy, contract: ContractAddress, id: TransactionID,
    ) {
        self.assert_event_tx_executed(contract, id);
        self.assert_no_events_left_from(contract);
    }

    //
    // CallSalt
    //

    fn assert_event_call_salt(
        ref self: EventSpy, contract: ContractAddress, id: TransactionID, salt: felt252,
    ) {
        let expected = Event::CallSalt(CallSalt { id, salt });
        self.assert_emitted_single(contract, expected);
    }
}
