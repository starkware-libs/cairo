use core::hash::{HashStateExTrait, HashStateTrait};
use core::poseidon::PoseidonTrait;
use openzeppelin_test_common::mocks::erc20::DualCaseERC20PermitMock;
use openzeppelin_test_common::mocks::erc20::DualCaseERC20PermitMock::SNIP12MetadataImpl;
use openzeppelin_testing as utils;
use openzeppelin_testing::constants;
use openzeppelin_testing::signing::{StarkKeyPair, StarkSerializedSigning};
use openzeppelin_utils::cryptography::snip12::{StarknetDomain, StructHash};
use snforge_std::signature::stark_curve::StarkCurveSignerImpl;
use snforge_std::{
    start_cheat_block_timestamp, start_cheat_caller_address, start_cheat_chain_id_global,
    test_address,
};
use starknet::ContractAddress;
use crate::erc20::ERC20Component::{
    ERC20MixinImpl, ERC20PermitImpl, InternalImpl, SNIP12MetadataExternalImpl,
};
use crate::erc20::snip12_utils::permit::{PERMIT_TYPE_HASH, Permit};
use crate::erc20::{DefaultConfig, ERC20Component};

//
// Constants
//

#[derive(Copy, Drop)]
struct TestData {
    token_address: ContractAddress,
    owner: ContractAddress,
    key_pair: StarkKeyPair,
    spender: ContractAddress,
    amount: u256,
    deadline: u64,
    token_supply: u256,
    name: @ByteArray,
    symbol: @ByteArray,
    metadata_name: felt252,
    metadata_version: felt252,
    chain_id: felt252,
    revision: felt252,
}

fn TEST_DATA() -> TestData {
    TestData {
        token_address: test_address(),
        owner: constants::OWNER,
        key_pair: constants::stark::KEY_PAIR(),
        spender: constants::SPENDER,
        amount: constants::TOKEN_VALUE,
        deadline: constants::TIMESTAMP,
        token_supply: constants::SUPPLY,
        name: @constants::NAME(),
        symbol: @constants::SYMBOL(),
        metadata_name: DualCaseERC20PermitMock::SNIP12MetadataImpl::name(),
        metadata_version: DualCaseERC20PermitMock::SNIP12MetadataImpl::version(),
        chain_id: constants::CHAIN_ID,
        revision: 1 // As in the current SNIP-12 implementation
    }
}

//
// Setup
//

type ComponentState = ERC20Component::ComponentState<DualCaseERC20PermitMock::ContractState>;

fn COMPONENT_STATE() -> ComponentState {
    ERC20Component::component_state_for_testing()
}

fn setup(data: TestData) -> ComponentState {
    start_cheat_chain_id_global(data.chain_id);

    utils::declare_and_deploy_at(
        "DualCaseAccountMock", data.owner, array![data.key_pair.public_key],
    );

    let mut state = COMPONENT_STATE();
    state.initializer(data.name.clone(), data.symbol.clone());
    state.mint(data.owner, data.token_supply);

    state
}

//
// IERC20Permit
//

#[test]
fn test_valid_permit_default_data() {
    let data = TEST_DATA();
    let (owner, spender, amount, deadline) = (data.owner, data.spender, data.amount, data.deadline);
    let mut state = setup(data);

    assert_valid_allowance(owner, spender, 0);

    let nonce = state.nonces(owner);
    let signature = prepare_permit_signature(data, nonce);
    state.permit(owner, spender, amount, deadline, signature);

    assert_valid_allowance(owner, spender, amount);
    assert_valid_nonce(owner, nonce + 1);
}

#[test]
fn test_valid_permit_other_data() {
    let mut data = TEST_DATA();
    data.spender = constants::OTHER;
    data.amount = constants::TOKEN_VALUE_2;
    let (owner, spender, amount, deadline) = (data.owner, data.spender, data.amount, data.deadline);
    let mut state = setup(data);

    assert_valid_allowance(owner, spender, 0);

    let nonce = state.nonces(owner);
    let signature = prepare_permit_signature(data, nonce);
    state.permit(owner, spender, amount, deadline, signature);

    assert_valid_allowance(owner, spender, amount);
    assert_valid_nonce(owner, nonce + 1);
}

#[test]
fn test_spend_permit() {
    let data = TEST_DATA();
    let (owner, spender, amount, deadline) = (data.owner, data.spender, data.amount, data.deadline);
    let mut state = setup(data);

    let nonce = state.nonces(owner);
    let signature = prepare_permit_signature(data, nonce);
    start_cheat_caller_address(data.token_address, spender);

    state.permit(owner, spender, amount, deadline, signature);
    state.transfer_from(owner, spender, amount);

    assert_valid_balance(spender, amount);
    assert_valid_balance(owner, data.token_supply - amount);
    assert_valid_allowance(owner, spender, 0);
    assert_valid_nonce(owner, nonce + 1);
}

#[test]
fn test_spend_half_permit() {
    let data = TEST_DATA();
    let (owner, spender, amount, deadline) = (data.owner, data.spender, data.amount, data.deadline);
    let mut state = setup(data);

    let nonce = state.nonces(owner);
    let signature = prepare_permit_signature(data, nonce);
    start_cheat_caller_address(data.token_address, spender);

    state.permit(owner, spender, amount, deadline, signature);
    let transfer_amount = amount / 2;
    state.transfer_from(owner, spender, transfer_amount);

    assert_valid_balance(spender, transfer_amount);
    assert_valid_balance(owner, data.token_supply - transfer_amount);
    assert_valid_allowance(owner, spender, amount - transfer_amount);
    assert_valid_nonce(owner, nonce + 1);
}

#[test]
fn test_subsequent_permits() {
    let mut data = TEST_DATA();
    let (owner, spender, amount_1, deadline) = (
        data.owner, data.spender, data.amount, data.deadline,
    );
    let mut state = setup(data);

    let mut expected_owner_balance = data.token_supply;
    let mut expected_spender_balance = 0;
    start_cheat_caller_address(data.token_address, spender);

    // Permit 1
    let nonce_1 = state.nonces(owner);
    let signature_1 = prepare_permit_signature(data, nonce_1);

    state.permit(owner, spender, amount_1, deadline, signature_1);
    state.transfer_from(owner, spender, amount_1);

    expected_owner_balance -= amount_1;
    expected_spender_balance += amount_1;
    assert_valid_balance(owner, expected_owner_balance);
    assert_valid_balance(spender, expected_spender_balance);
    assert_valid_allowance(owner, spender, 0);
    assert_valid_nonce(owner, nonce_1 + 1);

    // Permit 2
    data.amount = constants::TOKEN_VALUE_2;
    let amount_2 = data.amount;
    let nonce_2 = 1;
    let signature_2 = prepare_permit_signature(data, nonce_2);

    state.permit(owner, spender, amount_2, deadline, signature_2);
    state.transfer_from(owner, spender, amount_2);

    expected_owner_balance -= amount_2;
    expected_spender_balance += amount_2;
    assert_valid_balance(owner, expected_owner_balance);
    assert_valid_balance(spender, expected_spender_balance);
    assert_valid_allowance(owner, spender, 0);
    assert_valid_nonce(owner, nonce_2 + 1);
}

#[test]
#[should_panic(expected: 'ERC20: invalid permit signature')]
fn test_replay_attack() {
    let data = TEST_DATA();
    let (owner, spender, amount, deadline) = (data.owner, data.spender, data.amount, data.deadline);
    let mut state = setup(data);

    let nonce = state.nonces(owner);
    start_cheat_caller_address(data.token_address, spender);

    // 1st call is fine
    let signature = prepare_permit_signature(data, nonce);
    state.permit(owner, spender, amount, deadline, signature);

    // 2nd call must fail (nonce already used)
    let signature = prepare_permit_signature(data, nonce);
    state.permit(owner, spender, amount, deadline, signature);
}

#[test]
fn test_domain_separator() {
    let data = TEST_DATA();
    let state = setup(data);

    let sn_domain = StarknetDomain {
        name: data.metadata_name,
        version: data.metadata_version,
        chain_id: data.chain_id,
        revision: data.revision,
    };
    let expected_domain_separator = sn_domain.hash_struct();
    assert_eq!(state.DOMAIN_SEPARATOR(), expected_domain_separator);
}

//
// SNIP12Metadata
//

#[test]
fn test_permit_type_hash() {
    let expected_type_hash = selector!(
        "\"Permit\"(\"token\":\"ContractAddress\",\"spender\":\"ContractAddress\",\"amount\":\"u256\",\"nonce\":\"felt\",\"deadline\":\"u128\")\"u256\"(\"low\":\"u128\",\"high\":\"u128\")",
    );
    assert_eq!(PERMIT_TYPE_HASH, expected_type_hash);
}

#[test]
fn test_snip12_metadata() {
    let data = TEST_DATA();
    let state = setup(data);

    let (metadata_name, metadata_version) = state.snip12_metadata();
    assert_eq!(metadata_name, data.metadata_name);
    assert_eq!(metadata_version, data.metadata_version);
}

//
// Invalid signature
//

#[test]
#[should_panic(expected: 'ERC20: invalid permit signature')]
fn test_invalid_sig_bad_owner() {
    let data = TEST_DATA();
    let (spender, amount, deadline) = (data.spender, data.amount, data.deadline);
    let mut state = setup(data);

    let another_account = constants::OTHER;
    utils::deploy_another_at(data.owner, another_account, array![data.key_pair.public_key]);
    let nonce = state.nonces(another_account);
    let signature = prepare_permit_signature(data, nonce);
    state.permit(another_account, spender, amount, deadline, signature);
}

#[test]
#[should_panic(expected: 'ERC20: invalid permit signature')]
fn test_invalid_sig_bad_token_address() {
    let data = TEST_DATA();
    let (owner, spender, amount, deadline) = (data.owner, data.spender, data.amount, data.deadline);
    let mut state = setup(data);

    let mut modified_data = data;
    modified_data.token_address = constants::OTHER;
    let nonce = state.nonces(owner);
    let signature = prepare_permit_signature(modified_data, nonce);
    state.permit(owner, spender, amount, deadline, signature);
}

#[test]
#[should_panic(expected: 'ERC20: invalid permit signature')]
fn test_invalid_sig_bad_spender() {
    let data = TEST_DATA();
    let (owner, spender, amount, deadline) = (data.owner, data.spender, data.amount, data.deadline);
    let mut state = setup(data);

    let mut modified_data = data;
    modified_data.spender = constants::OTHER;
    let nonce = state.nonces(owner);
    let signature = prepare_permit_signature(modified_data, nonce);
    state.permit(owner, spender, amount, deadline, signature);
}

#[test]
#[should_panic(expected: 'ERC20: invalid permit signature')]
fn test_invalid_sig_bad_amount() {
    let data = TEST_DATA();
    let (owner, spender, amount, deadline) = (data.owner, data.spender, data.amount, data.deadline);
    let mut state = setup(data);

    let mut modified_data = data;
    modified_data.amount = constants::TOKEN_VALUE_2;
    let nonce = state.nonces(owner);
    let signature = prepare_permit_signature(modified_data, nonce);
    state.permit(owner, spender, amount, deadline, signature);
}

#[test]
#[should_panic(expected: 'ERC20: invalid permit signature')]
fn test_invalid_sig_bad_nonce() {
    let data = TEST_DATA();
    let (owner, spender, amount, deadline) = (data.owner, data.spender, data.amount, data.deadline);
    let mut state = setup(data);

    let another_nonce = 1 + state.nonces(owner);
    let signature = prepare_permit_signature(data, another_nonce);
    state.permit(owner, spender, amount, deadline, signature);
}

#[test]
#[should_panic(expected: 'ERC20: invalid permit signature')]
fn test_invalid_sig_bad_sig_r() {
    let data = TEST_DATA();
    let (owner, spender, amount, deadline) = (data.owner, data.spender, data.amount, data.deadline);
    let mut state = setup(data);

    let nonce = state.nonces(owner);
    let signature = prepare_permit_signature(data, nonce);
    let (sig_r, sig_s) = (*signature.at(0), *signature.at(1));
    let modified_signature = array![sig_r + 1, sig_s].span();
    state.permit(owner, spender, amount, deadline, modified_signature);
}

#[test]
#[should_panic(expected: 'ERC20: invalid permit signature')]
fn test_invalid_sig_bad_sig_s() {
    let data = TEST_DATA();
    let (owner, spender, amount, deadline) = (data.owner, data.spender, data.amount, data.deadline);
    let mut state = setup(data);

    let nonce = state.nonces(owner);
    let signature = prepare_permit_signature(data, nonce);
    let (sig_r, sig_s) = (*signature.at(0), *signature.at(1));
    let modified_signature = array![sig_r, sig_s + 1].span();
    state.permit(owner, spender, amount, deadline, modified_signature);
}

#[test]
#[should_panic(expected: 'ERC20: invalid permit signature')]
fn test_invalid_sig_bad_metadata_name() {
    let data = TEST_DATA();
    let (owner, spender, amount, deadline) = (data.owner, data.spender, data.amount, data.deadline);
    let mut state = setup(data);

    let mut modified_data = data;
    modified_data.metadata_name = 'ANOTHER_NAME';
    let nonce = state.nonces(owner);
    let signature = prepare_permit_signature(modified_data, nonce);
    state.permit(owner, spender, amount, deadline, signature);
}

#[test]
#[should_panic(expected: 'ERC20: invalid permit signature')]
fn test_invalid_sig_bad_metadata_version() {
    let data = TEST_DATA();
    let (owner, spender, amount, deadline) = (data.owner, data.spender, data.amount, data.deadline);
    let mut state = setup(data);

    let mut modified_data = data;
    modified_data.metadata_version = 'ANOTHER_VERSION';
    let nonce = state.nonces(owner);
    let signature = prepare_permit_signature(modified_data, nonce);
    state.permit(owner, spender, amount, deadline, signature);
}

#[test]
#[should_panic(expected: 'ERC20: invalid permit signature')]
fn test_invalid_sig_bad_signing_key() {
    let data = TEST_DATA();
    let (owner, spender, amount, deadline) = (data.owner, data.spender, data.amount, data.deadline);
    let mut state = setup(data);

    let mut modified_data = data;
    modified_data.key_pair = constants::stark::KEY_PAIR_2();
    let nonce = state.nonces(owner);
    let signature = prepare_permit_signature(modified_data, nonce);
    state.permit(owner, spender, amount, deadline, signature);
}

#[test]
#[should_panic(expected: 'ERC20: invalid permit signature')]
fn test_invalid_sig_bad_chain_id() {
    let data = TEST_DATA();
    let (owner, spender, amount, deadline) = (data.owner, data.spender, data.amount, data.deadline);
    let mut state = setup(data);

    let mut modified_data = data;
    modified_data.chain_id = 'ANOTHER_CHAIN_ID';
    let nonce = state.nonces(owner);
    let signature = prepare_permit_signature(modified_data, nonce);
    state.permit(owner, spender, amount, deadline, signature);
}

#[test]
#[should_panic(expected: 'ERC20: invalid permit signature')]
fn test_invalid_sig_bad_revision() {
    let data = TEST_DATA();
    let (owner, spender, amount, deadline) = (data.owner, data.spender, data.amount, data.deadline);
    let mut state = setup(data);

    let mut modified_data = data;
    modified_data.revision = 'ANOTHER_REVISION';
    let nonce = state.nonces(owner);
    let signature = prepare_permit_signature(modified_data, nonce);
    state.permit(owner, spender, amount, deadline, signature);
}

//
// Expired signature
//

#[test]
#[should_panic(expected: 'ERC20: expired permit signature')]
fn test_invalid_sig_bad_deadline() {
    let data = TEST_DATA();
    let (owner, spender, amount, deadline) = (data.owner, data.spender, data.amount, data.deadline);
    let mut state = setup(data);

    let timestamp_after_deadline = deadline + 1;
    start_cheat_block_timestamp(data.token_address, timestamp_after_deadline);
    let nonce = state.nonces(owner);
    let signature = prepare_permit_signature(data, nonce);
    state.permit(owner, spender, amount, deadline, signature);
}

//
// Helpers
//

fn prepare_permit_signature(data: TestData, nonce: felt252) -> Span<felt252> {
    let sn_domain = StarknetDomain {
        name: data.metadata_name,
        version: data.metadata_version,
        chain_id: data.chain_id,
        revision: data.revision,
    };
    let permit = Permit {
        token: data.token_address,
        spender: data.spender,
        amount: data.amount,
        nonce,
        deadline: data.deadline,
    };
    let msg_hash = PoseidonTrait::new()
        .update_with('StarkNet Message')
        .update_with(sn_domain.hash_struct())
        .update_with(data.owner)
        .update_with(permit.hash_struct())
        .finalize();

    data.key_pair.serialized_sign(msg_hash).span()
}

fn assert_valid_nonce(account: ContractAddress, expected: felt252) {
    assert_eq!(COMPONENT_STATE().nonces(account), expected);
}

fn assert_valid_allowance(owner: ContractAddress, spender: ContractAddress, expected: u256) {
    assert_eq!(COMPONENT_STATE().allowance(owner, spender), expected);
}

fn assert_valid_balance(account: ContractAddress, expected: u256) {
    assert_eq!(COMPONENT_STATE().balance_of(account), expected);
}
