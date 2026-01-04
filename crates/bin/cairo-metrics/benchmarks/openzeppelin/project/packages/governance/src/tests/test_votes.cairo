use openzeppelin_test_common::mocks::votes::ERC721VotesMock::SNIP12MetadataImpl;
use openzeppelin_test_common::mocks::votes::{ERC20VotesMock, ERC721VotesMock};
use openzeppelin_testing as utils;
use openzeppelin_testing::constants::{DELEGATEE, DELEGATOR, OTHER, RECIPIENT, SUPPLY, ZERO};
use openzeppelin_testing::{AsAddressTrait, EventSpyExt, EventSpyQueue as EventSpy, spy_events};
use openzeppelin_token::erc20::ERC20Component::InternalTrait;
use openzeppelin_token::erc20::interface::IERC20;
use openzeppelin_token::erc721::ERC721Component::{
    ERC721CamelOnlyImpl, ERC721Impl, ERC721MetadataImpl, InternalImpl as ERC721InternalImpl,
};
use openzeppelin_token::erc721::interface::IERC721;
use openzeppelin_utils::cryptography::snip12::OffchainMessageHash;
use openzeppelin_utils::structs::checkpoint::TraceTrait;
use snforge_std::signature::stark_curve::{StarkCurveKeyPairImpl, StarkCurveSignerImpl};
use snforge_std::{
    start_cheat_block_timestamp_global, start_cheat_caller_address, start_cheat_chain_id_global,
    test_address,
};
use starknet::ContractAddress;
use starknet::storage::StoragePathEntry;
use crate::votes::VotesComponent::{
    DelegateChanged, DelegateVotesChanged, InternalImpl, VotesImpl, VotingUnitsTrait,
};
use crate::votes::{Delegation, VotesComponent};

const ERC721_INITIAL_MINT: u256 = 10;

//
// Setup
//

type ComponentState = VotesComponent::ComponentState<ERC721VotesMock::ContractState>;
type ERC20ComponentState = VotesComponent::ComponentState<ERC20VotesMock::ContractState>;

fn COMPONENT_STATE() -> ComponentState {
    VotesComponent::component_state_for_testing()
}

fn ERC20_COMPONENT_STATE() -> ERC20ComponentState {
    VotesComponent::component_state_for_testing()
}

fn ERC721VOTES_CONTRACT_STATE() -> ERC721VotesMock::ContractState {
    ERC721VotesMock::contract_state_for_testing()
}

fn ERC20VOTES_CONTRACT_STATE() -> ERC20VotesMock::ContractState {
    ERC20VotesMock::contract_state_for_testing()
}

fn setup_erc721_votes() -> ComponentState {
    let mut state = COMPONENT_STATE();
    let mut mock_state = ERC721VOTES_CONTRACT_STATE();
    // Mint ERC721_INITIAL_MINT NFTs to DELEGATOR
    for i in 0..ERC721_INITIAL_MINT {
        mock_state.erc721.mint(DELEGATOR, i);
    }
    state
}

fn setup_erc20_votes() -> ERC20ComponentState {
    let mut state = ERC20_COMPONENT_STATE();
    let mut mock_state = ERC20VOTES_CONTRACT_STATE();

    // Mint SUPPLY tokens to DELEGATOR
    mock_state.erc20.mint(DELEGATOR, SUPPLY);
    state
}

fn setup_account(public_key: felt252) -> ContractAddress {
    let mut calldata = array![public_key];
    utils::declare_and_deploy("SnakeAccountMock", calldata)
}

//
// Common tests for Votes
//

//
// get_votes
//

#[test]
fn test_get_votes() {
    let mut state = setup_erc721_votes();
    start_cheat_caller_address(test_address(), DELEGATOR);
    // Before delegating, the DELEGATOR has 0 votes
    assert_eq!(state.get_votes(DELEGATOR), 0);
    state.delegate(DELEGATOR);

    assert_eq!(state.get_votes(DELEGATOR), ERC721_INITIAL_MINT);
}

//
// get_past_votes
//

#[test]
fn test_get_past_votes() {
    let mut state = setup_erc721_votes();
    let mut trace = state.Votes_delegate_checkpoints.entry(DELEGATOR);

    start_cheat_block_timestamp_global('ts10');

    trace.push('ts1', 3);
    trace.push('ts2', 5);
    trace.push('ts3', 7);

    assert_eq!(state.get_past_votes(DELEGATOR, 'ts1'), 3);
    assert_eq!(state.get_past_votes(DELEGATOR, 'ts2'), 5);
    assert_eq!(state.get_past_votes(DELEGATOR, 'ts5'), 7);
    // This is because we had not delegated at 'ts0'
    assert_eq!(state.get_past_votes(DELEGATOR, 'ts0'), 0);
}

#[test]
#[should_panic(expected: 'Votes: future Lookup')]
fn test_get_past_votes_future_lookup() {
    let state = setup_erc721_votes();

    start_cheat_block_timestamp_global('ts1');
    state.get_past_votes(DELEGATOR, 'ts2');
}

//
// get_past_total_supply
//

#[test]
fn test_get_past_total_supply() {
    let mut state = setup_erc721_votes();
    let mut trace = state.Votes_total_checkpoints.deref();

    start_cheat_block_timestamp_global('ts10');
    trace.push('ts1', 3);
    trace.push('ts2', 5);
    trace.push('ts3', 7);

    // At ts 'ts0', the total supply is the initial mint
    assert_eq!(state.get_past_total_supply('ts0'), ERC721_INITIAL_MINT);
    assert_eq!(state.get_past_total_supply('ts1'), 3);
    assert_eq!(state.get_past_total_supply('ts2'), 5);
    assert_eq!(state.get_past_total_supply('ts5'), 7);
}

#[test]
fn test_get_past_total_supply_before_checkpoints() {
    start_cheat_block_timestamp_global('ts1');
    let mut state = setup_erc721_votes();
    let mut trace = state.Votes_total_checkpoints.deref();

    start_cheat_block_timestamp_global('ts10');
    trace.push('ts1', 3);
    trace.push('ts2', 5);

    assert_eq!(state.get_past_total_supply('ts0'), 0);
}

#[test]
#[should_panic(expected: 'Votes: future Lookup')]
fn test_get_past_total_supply_future_lookup() {
    let state = setup_erc721_votes();
    start_cheat_block_timestamp_global('ts1');
    state.get_past_total_supply('ts2');
}

//
// delegates
//

#[test]
fn test_delegates() {
    let mut state = setup_erc721_votes();
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, DELEGATOR);

    state.delegate(DELEGATOR);
    assert_eq!(state.delegates(DELEGATOR), DELEGATOR);
}

//
// delegate
//

#[test]
fn test_self_delegate() {
    let mut state = setup_erc721_votes();
    let contract_address = test_address();
    let mut spy = spy_events();
    start_cheat_caller_address(contract_address, DELEGATOR);

    state.delegate(DELEGATOR);
    spy.assert_event_delegate_changed(contract_address, DELEGATOR, ZERO, DELEGATOR);
    spy
        .assert_only_event_delegate_votes_changed(
            contract_address, DELEGATOR, 0, ERC721_INITIAL_MINT,
        );
    assert_eq!(state.get_votes(DELEGATOR), ERC721_INITIAL_MINT);
}

#[test]
fn test_delegate_to_delegatee_updates_votes() {
    let mut state = setup_erc721_votes();
    let contract_address = test_address();
    let mut spy = spy_events();
    start_cheat_caller_address(contract_address, DELEGATOR);

    state.delegate(DELEGATEE);
    spy.assert_event_delegate_changed(contract_address, DELEGATOR, ZERO, DELEGATEE);
    spy
        .assert_only_event_delegate_votes_changed(
            contract_address, DELEGATEE, 0, ERC721_INITIAL_MINT,
        );
    assert_eq!(state.get_votes(DELEGATEE), ERC721_INITIAL_MINT);
    assert_eq!(state.get_votes(DELEGATOR), 0);
}

#[test]
fn test_delegate_to_delegatee_updates_delegates() {
    let mut state = setup_erc721_votes();
    start_cheat_caller_address(test_address(), DELEGATOR);
    state.delegate(DELEGATOR);
    assert_eq!(state.delegates(DELEGATOR), DELEGATOR);
    state.delegate(DELEGATEE);
    assert_eq!(state.delegates(DELEGATOR), DELEGATEE);
}

#[test]
fn test_delegate_with_no_balance() {
    let mut state = setup_erc721_votes();
    let contract_address = test_address();
    let mut spy = spy_events();
    start_cheat_caller_address(contract_address, OTHER);

    // OTHER has no balance, so delegating should not change any votes
    state.delegate(DELEGATEE);

    spy.assert_event_delegate_changed(contract_address, OTHER, ZERO, DELEGATEE);
    // No DelegateVotesChanged event should be emitted
    spy.assert_no_events_left_from(contract_address);

    assert_eq!(state.get_votes(DELEGATEE), 0);
    assert_eq!(state.get_votes(OTHER), 0);
    assert_eq!(state.delegates(OTHER), DELEGATEE);
}

//
// delegate_by_sig
//

#[test]
fn test_delegate_by_sig() {
    // Set up the state
    // start_cheat_chain_id_global('SN_TEST');
    let mut state = setup_erc721_votes();
    let contract_address = test_address();
    start_cheat_block_timestamp_global('ts1');

    // Generate a key pair and set up an account
    let key_pair = StarkCurveKeyPairImpl::generate();
    let account = setup_account(key_pair.public_key);

    // Set up delegation parameters
    let nonce = 0;
    let expiry = 'ts2';
    let verifying_contract = contract_address;
    let delegator = account;
    let delegatee = DELEGATEE;

    // Create and sign the delegation message
    let delegation = Delegation { verifying_contract, delegatee, nonce, expiry };
    let msg_hash = delegation.get_message_hash(delegator);
    let (r, s) = key_pair.sign(msg_hash).unwrap();

    // Set up event spy and execute delegation
    let mut spy = spy_events();
    state.delegate_by_sig(delegator, delegatee, nonce, expiry, array![r, s].span());

    spy.assert_only_event_delegate_changed(contract_address, delegator, ZERO, delegatee);
    assert_eq!(state.delegates(account), delegatee);
}

#[test]
fn test_delegate_by_sig_hash_generation() {
    start_cheat_chain_id_global('SN_TEST');

    let nonce = 0;
    let expiry = 'ts2';
    let delegator = 0x70b0526a4bfbc9ca717c96aeb5a8afac85181f4585662273668928585a0d628.as_address();
    let verifying_contract = 'VERIFIER'.as_address();
    let delegatee = RECIPIENT;
    let delegation = Delegation { verifying_contract, delegatee, nonce, expiry };

    let hash = delegation.get_message_hash(delegator);

    // This hash was computed using starknet js sdk from the following values:
    // - name: 'DAPP_NAME'
    // - version: 'DAPP_VERSION'
    // - chainId: 'SN_TEST'
    // - account: 0x70b0526a4bfbc9ca717c96aeb5a8afac85181f4585662273668928585a0d628
    // - verifying_contract: 'VERIFIER'
    // - delegatee: 'RECIPIENT'
    // - nonce: 0
    // - expiry: 'ts2'
    // - revision: '1'
    let expected_hash = 0x1fa1af6d3d0ede7d09790ef20a894668af9445b6bc93714e87a758be40efdc7;
    assert_eq!(hash, expected_hash);
}

#[test]
#[should_panic(expected: 'Votes: expired signature')]
fn test_delegate_by_sig_past_expiry() {
    start_cheat_block_timestamp_global('ts5');

    let mut state = setup_erc721_votes();
    let expiry = 'ts4';
    let signature = array![0, 0];

    state.delegate_by_sig(DELEGATOR, DELEGATEE, 0, expiry, signature.span());
}

#[test]
#[should_panic(expected: 'Nonces: invalid nonce')]
fn test_delegate_by_sig_invalid_nonce() {
    let mut state = setup_erc721_votes();
    let signature = array![0, 0];

    state.delegate_by_sig(DELEGATOR, DELEGATEE, 1, 0, signature.span());
}

#[test]
#[should_panic(expected: 'Votes: invalid signature')]
fn test_delegate_by_sig_invalid_signature() {
    let mut state = setup_erc721_votes();
    let key_pair = StarkCurveKeyPairImpl::generate();
    let account = setup_account(key_pair.public_key);

    let nonce = 0;
    let expiry = 'ts2';
    let delegator = account;
    let delegatee = DELEGATEE;
    let verifying_contract = test_address();
    let delegation = Delegation { verifying_contract, delegatee, nonce, expiry };
    let msg_hash = delegation.get_message_hash(delegator);
    let (r, s) = key_pair.sign(msg_hash).unwrap();

    start_cheat_block_timestamp_global('ts1');
    // Use an invalid signature
    state.delegate_by_sig(delegator, delegatee, nonce, expiry, array![r + 1, s].span());
}

#[test]
#[should_panic(expected: 'Votes: invalid signature')]
fn test_delegate_by_sig_bad_delegatee() {
    let mut state = setup_erc721_votes();
    let key_pair = StarkCurveKeyPairImpl::generate();
    let account = setup_account(key_pair.public_key);

    let nonce = 0;
    let expiry = 'ts2';
    let delegator = account;
    let delegatee = DELEGATEE;
    let verifying_contract = test_address();
    let bad_delegatee = 0x1234.as_address();
    let delegation = Delegation { verifying_contract, delegatee, nonce, expiry };
    let msg_hash = delegation.get_message_hash(delegator);
    let (r, s) = key_pair.sign(msg_hash).unwrap();

    start_cheat_block_timestamp_global('ts1');
    // Use a different delegatee than the one signed for
    state.delegate_by_sig(delegator, bad_delegatee, nonce, expiry, array![r, s].span());
}

#[test]
#[should_panic(expected: 'Nonces: invalid nonce')]
fn test_delegate_by_sig_reused_signature() {
    let mut state = setup_erc721_votes();
    let key_pair = StarkCurveKeyPairImpl::generate();
    let account = setup_account(key_pair.public_key);

    let nonce = 0;
    let expiry = 'ts2';
    let delegator = account;
    let delegatee = DELEGATEE;
    let verifying_contract = test_address();
    let delegation = Delegation { verifying_contract, delegatee, nonce, expiry };
    let msg_hash = delegation.get_message_hash(delegator);
    let (r, s) = key_pair.sign(msg_hash).unwrap();

    start_cheat_block_timestamp_global('ts1');
    // First delegation (should succeed)
    state.delegate_by_sig(delegator, delegatee, nonce, expiry, array![r, s].span());

    // Attempt to reuse the same signature (should fail)
    state.delegate_by_sig(delegator, delegatee, nonce, expiry, array![r, s].span());
}

//
// num_checkpoints
//

#[test]
fn test_num_checkpoints() {
    let mut state = setup_erc721_votes();
    let mut trace = state.Votes_delegate_checkpoints.entry(DELEGATOR);

    start_cheat_block_timestamp_global('ts10');

    trace.push('ts1', 3);
    trace.push('ts2', 5);
    trace.push('ts3', 7);

    assert_eq!(state.num_checkpoints(DELEGATOR), 3);
    assert_eq!(state.num_checkpoints(OTHER), 0);
}

//
// checkpoints
//

#[test]
fn test_checkpoints() {
    let mut state = setup_erc721_votes();
    let mut trace = state.Votes_delegate_checkpoints.entry(DELEGATOR);

    start_cheat_block_timestamp_global('ts10');

    trace.push('ts1', 3);
    trace.push('ts2', 5);
    trace.push('ts3', 7);

    let checkpoint0 = state.checkpoints(DELEGATOR, 0);
    assert_eq!(checkpoint0.key, 'ts1');
    assert_eq!(checkpoint0.value, 3);

    let checkpoint1 = state.checkpoints(DELEGATOR, 1);
    assert_eq!(checkpoint1.key, 'ts2');
    assert_eq!(checkpoint1.value, 5);

    let checkpoint2 = state.checkpoints(DELEGATOR, 2);
    assert_eq!(checkpoint2.key, 'ts3');
    assert_eq!(checkpoint2.value, 7);
}

//
// Tests specific to ERC721Votes and ERC20Votes
//

#[test]
fn test_erc721_get_voting_units() {
    let state = setup_erc721_votes();

    assert_eq!(state.get_voting_units(DELEGATOR), ERC721_INITIAL_MINT);
    assert_eq!(state.get_voting_units(OTHER), 0);
}

#[test]
fn test_erc20_get_voting_units() {
    let mut state = setup_erc20_votes();

    assert_eq!(state.get_voting_units(DELEGATOR), SUPPLY);
    assert_eq!(state.get_voting_units(OTHER), 0);
}

#[test]
fn test_erc20_burn_updates_votes() {
    let mut state = setup_erc20_votes();
    let mut mock_state = ERC20VOTES_CONTRACT_STATE();
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, DELEGATOR);
    start_cheat_block_timestamp_global('ts1');

    state.delegate(DELEGATOR);

    // Set spy and burn some tokens
    let mut spy = spy_events();
    let burn_amount = 1000;
    mock_state.erc20.burn(DELEGATOR, burn_amount);

    // We need to move the timestamp forward to be able to call get_past_total_supply
    start_cheat_block_timestamp_global('ts2');
    spy
        .assert_event_delegate_votes_changed(
            contract_address, DELEGATOR, SUPPLY, SUPPLY - burn_amount,
        );
    assert_eq!(state.get_votes(DELEGATOR), SUPPLY - burn_amount);
    assert_eq!(state.get_past_total_supply('ts1'), SUPPLY - burn_amount);
}

#[test]
fn test_erc721_burn_updates_votes() {
    let mut state = setup_erc721_votes();
    let mut mock_state = ERC721VOTES_CONTRACT_STATE();
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, DELEGATOR);
    start_cheat_block_timestamp_global('ts1');

    state.delegate(DELEGATOR);

    // Set spy and burn some tokens
    let mut spy = spy_events();
    let burn_amount = 3;
    for i in 0..burn_amount {
        mock_state.erc721.burn(i);
        spy
            .assert_event_delegate_votes_changed(
                contract_address, DELEGATOR, ERC721_INITIAL_MINT - i, ERC721_INITIAL_MINT - i - 1,
            );
    }

    // We need to move the timestamp forward to be able to call get_past_total_supply
    start_cheat_block_timestamp_global('ts2');
    assert_eq!(state.get_votes(DELEGATOR), ERC721_INITIAL_MINT - burn_amount);
    assert_eq!(state.get_past_total_supply('ts1'), ERC721_INITIAL_MINT - burn_amount);
}

#[test]
fn test_erc_721_get_total_supply() {
    let state = setup_erc721_votes();
    assert_eq!(state.get_total_supply(), ERC721_INITIAL_MINT);
}

#[test]
fn test_erc_20_get_total_supply() {
    let state = setup_erc20_votes();
    assert_eq!(state.get_total_supply(), SUPPLY);
}

#[test]
fn test_erc_20_voting_units_update_with_full_balance_transfer() {
    let mut state = setup_erc20_votes();
    let mut mock_state = ERC20VOTES_CONTRACT_STATE();
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, DELEGATOR);

    // DELEGATOR self-delegates
    state.delegate(DELEGATOR);
    assert_eq!(state.get_votes(DELEGATOR), SUPPLY);

    let mut spy = spy_events();

    // Full balance transfer
    mock_state.erc20.transfer(RECIPIENT, SUPPLY);

    spy.assert_event_delegate_votes_changed(contract_address, DELEGATOR, SUPPLY, 0);
    assert_eq!(state.get_votes(DELEGATOR), 0);
    assert_eq!(state.get_votes(RECIPIENT), 0); // RECIPIENT hasn't delegated yet

    // RECIPIENT delegates to themselves
    start_cheat_caller_address(contract_address, RECIPIENT);
    state.delegate(RECIPIENT);

    spy.assert_event_delegate_votes_changed(contract_address, RECIPIENT, 0, SUPPLY);
    assert_eq!(state.get_votes(RECIPIENT), SUPPLY);
}

#[test]
fn test_erc_20_voting_units_update_with_partial_balance_transfer() {
    let mut state = setup_erc20_votes();
    let mut mock_state = ERC20VOTES_CONTRACT_STATE();
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, DELEGATOR);

    // DELEGATOR self-delegates
    state.delegate(DELEGATOR);
    assert_eq!(state.get_votes(DELEGATOR), SUPPLY);

    let mut spy = spy_events();

    // Partial transfer
    let partial_amount = SUPPLY / 2;
    mock_state.erc20.transfer(RECIPIENT, partial_amount);

    spy
        .assert_event_delegate_votes_changed(
            contract_address, DELEGATOR, SUPPLY, SUPPLY - partial_amount,
        );
    assert_eq!(state.get_votes(DELEGATOR), SUPPLY - partial_amount);
    assert_eq!(state.get_votes(RECIPIENT), 0); // RECIPIENT hasn't delegated yet

    // RECIPIENT delegates to themselves
    start_cheat_caller_address(contract_address, RECIPIENT);
    state.delegate(RECIPIENT);

    spy.assert_event_delegate_votes_changed(contract_address, RECIPIENT, 0, partial_amount);
    assert_eq!(state.get_votes(RECIPIENT), partial_amount);
}

#[test]
fn test_erc721_voting_units_update_with_single_token_transfer() {
    let mut state = setup_erc721_votes();
    let mut mock_state = ERC721VOTES_CONTRACT_STATE();
    let contract_address = test_address();
    start_cheat_caller_address(contract_address, DELEGATOR);

    // DELEGATOR self-delegates
    state.delegate(DELEGATOR);
    assert_eq!(state.get_votes(DELEGATOR), ERC721_INITIAL_MINT);

    let mut spy = spy_events();

    // Transfer a single token
    let token_id = 0;
    mock_state.erc721.transfer_from(DELEGATOR, RECIPIENT, token_id);

    spy
        .assert_event_delegate_votes_changed(
            contract_address, DELEGATOR, ERC721_INITIAL_MINT, ERC721_INITIAL_MINT - 1,
        );

    assert_eq!(state.get_votes(DELEGATOR), ERC721_INITIAL_MINT - 1);
    assert_eq!(state.get_votes(RECIPIENT), 0); // RECIPIENT hasn't delegated yet

    // RECIPIENT delegates to themselves
    start_cheat_caller_address(contract_address, RECIPIENT);
    state.delegate(RECIPIENT);

    spy.assert_event_delegate_votes_changed(contract_address, RECIPIENT, 0, 1);
    assert_eq!(state.get_votes(RECIPIENT), 1);
}

//
// Helpers
//

#[generate_trait]
impl VotesSpyHelpersImpl of VotesSpyHelpers {
    fn assert_event_delegate_changed(
        ref self: EventSpy,
        contract: ContractAddress,
        delegator: ContractAddress,
        from_delegate: ContractAddress,
        to_delegate: ContractAddress,
    ) {
        let expected = VotesComponent::Event::DelegateChanged(
            DelegateChanged { delegator, from_delegate, to_delegate },
        );
        self.assert_emitted_single(contract, expected);
    }

    fn assert_event_delegate_votes_changed(
        ref self: EventSpy,
        contract: ContractAddress,
        delegate: ContractAddress,
        previous_votes: u256,
        new_votes: u256,
    ) {
        let expected = VotesComponent::Event::DelegateVotesChanged(
            DelegateVotesChanged { delegate, previous_votes, new_votes },
        );
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_delegate_changed(
        ref self: EventSpy,
        contract: ContractAddress,
        delegator: ContractAddress,
        from_delegate: ContractAddress,
        to_delegate: ContractAddress,
    ) {
        self.assert_event_delegate_changed(contract, delegator, from_delegate, to_delegate);
        self.assert_no_events_left_from(contract);
    }

    fn assert_only_event_delegate_votes_changed(
        ref self: EventSpy,
        contract: ContractAddress,
        delegate: ContractAddress,
        previous_votes: u256,
        new_votes: u256,
    ) {
        self.assert_event_delegate_votes_changed(contract, delegate, previous_votes, new_votes);
        self.assert_no_events_left_from(contract);
    }
}
