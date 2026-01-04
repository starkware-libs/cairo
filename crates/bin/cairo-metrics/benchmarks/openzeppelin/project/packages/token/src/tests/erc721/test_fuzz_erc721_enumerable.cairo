use openzeppelin_introspection::src5::SRC5Component::SRC5Impl;
use openzeppelin_test_common::mocks::erc721::ERC721EnumerableMock;
use openzeppelin_testing::constants::{OWNER, RECIPIENT};
use starknet::ContractAddress;
use starknet::storage::StorageMapReadAccess;
use crate::erc721::ERC721Component::{ERC721Impl, InternalImpl as ERC721InternalImpl};
use crate::erc721::extensions::erc721_enumerable::ERC721EnumerableComponent;
use crate::erc721::extensions::erc721_enumerable::ERC721EnumerableComponent::{
    ERC721EnumerableImpl, InternalImpl,
};

//
// Setup
//

type ComponentState =
    ERC721EnumerableComponent::ComponentState<ERC721EnumerableMock::ContractState>;

fn CONTRACT_STATE() -> ERC721EnumerableMock::ContractState {
    ERC721EnumerableMock::contract_state_for_testing()
}

fn COMPONENT_STATE() -> ComponentState {
    ERC721EnumerableComponent::component_state_for_testing()
}

const MIN_SUPPLY: u32 = 2;
const MAX_SUPPLY: u32 = 100;

fn prepare_supply(supply_seed: u32) -> u32 {
    MIN_SUPPLY + supply_seed % (MAX_SUPPLY - MIN_SUPPLY + 1)
}

fn setup(supply: u32) -> (ComponentState, Span<u256>) {
    let mut state = COMPONENT_STATE();
    let mut mock_state = CONTRACT_STATE();
    state.initializer();

    let mut tokens_list = array![];
    for i in 0..supply {
        let token_id = 'TOKEN'.into() + i.into();
        mock_state.erc721.mint(OWNER, token_id);
        tokens_list.append(token_id);
    }

    (state, tokens_list.span())
}

//
// Tests
//

#[test]
#[fuzzer]
fn test_initial_state(supply_seed: u32) {
    let supply = prepare_supply(supply_seed);
    let (_, tokens_list) = setup(supply);

    assert_total_supply(supply.into());
    assert_token_by_index(tokens_list);
    assert_token_of_owner_by_index(OWNER, tokens_list);
    assert_all_tokens_of_owner(OWNER, tokens_list);
}

#[test]
#[fuzzer]
fn test_burn_token(supply_seed: u32, burn_index_seed: u32) {
    let supply = prepare_supply(supply_seed);
    let (_, initial_list) = setup(supply);
    let mut state = CONTRACT_STATE();
    let burn_index = burn_index_seed % supply;
    let token_to_burn = *initial_list.at(burn_index);

    state.erc721.burn(token_to_burn);

    let expected_list = remove_token_from_list(initial_list, token_to_burn);
    assert_total_supply(supply - 1);
    assert_token_by_index(expected_list);
    assert_token_of_owner_by_index(OWNER, expected_list);
    assert_all_tokens_of_owner(OWNER, expected_list);
}

#[test]
#[fuzzer]
fn test_transfer_single_token(supply_seed: u32, index_seed: u32) {
    let supply = prepare_supply(supply_seed);
    let (_, initial_list) = setup(supply);
    let token_index = index_seed % supply;
    let token_id = *initial_list.at(token_index);
    let (owner, recipient) = (OWNER, RECIPIENT);
    let mut state = CONTRACT_STATE();

    state.erc721.transfer(owner, recipient, token_id);

    let expected_owner_tokens = remove_token_from_list(initial_list, token_id);
    let expected_recipient_tokens = array![token_id].span();
    assert_total_supply(supply);
    assert_token_by_index(initial_list);
    assert_token_of_owner_by_index(owner, expected_owner_tokens);
    assert_all_tokens_of_owner(owner, expected_owner_tokens);
    assert_token_of_owner_by_index(recipient, expected_recipient_tokens);
    assert_all_tokens_of_owner(recipient, expected_recipient_tokens);
}

#[test]
#[fuzzer]
fn test_transfer_two_tokens(supply_seed: u32, index_seed_1: u32, index_seed_2: u32) {
    let supply = prepare_supply(supply_seed);
    let token_index_1 = index_seed_1 % supply;
    let token_index_2 = index_seed_2 % supply;
    if token_index_1 == token_index_2 {
        return; // Doesn't test transfer of a same token
    }
    let (_, initial_list) = setup(supply);
    let token_1 = *initial_list.at(token_index_1);
    let token_2 = *initial_list.at(token_index_2);
    let (owner, recipient) = (OWNER, RECIPIENT);
    let mut state = CONTRACT_STATE();

    state.erc721.transfer(owner, recipient, token_1);
    state.erc721.transfer(owner, recipient, token_2);

    let expected_owner_tokens = remove_token_from_list(
        remove_token_from_list(initial_list, token_1), token_2,
    );
    let expected_recipient_tokens = array![token_1, token_2].span();
    assert_total_supply(supply);
    assert_token_by_index(initial_list);
    assert_token_of_owner_by_index(owner, expected_owner_tokens);
    assert_all_tokens_of_owner(owner, expected_owner_tokens);
    assert_token_of_owner_by_index(recipient, expected_recipient_tokens);
    assert_all_tokens_of_owner(recipient, expected_recipient_tokens);
}

#[test]
#[fuzzer]
fn test__add_token_to_owner_enumeration(supply_seed: u32, index_seed: u32) {
    let supply = prepare_supply(supply_seed);
    let (mut state, _) = setup(supply);
    let new_token_index = supply;
    let new_token_id = 'NEW_TOKEN'.into();

    assert_owner_tokens_index_to_id(OWNER, new_token_index, 0);
    assert_owner_tokens_id_to_index(new_token_id, 0);

    state._add_token_to_owner_enumeration(OWNER, new_token_id);

    assert_owner_tokens_index_to_id(OWNER, new_token_index, new_token_id);
    assert_owner_tokens_id_to_index(new_token_id, new_token_index);
}

#[test]
#[fuzzer]
fn test__add_token_to_all_tokens_enumeration(supply_seed: u32) {
    let initial_supply = prepare_supply(supply_seed);
    let (mut state, _) = setup(initial_supply);
    let new_token_index = initial_supply;
    let new_token_id = 'NEW_TOKEN'.into();

    assert_all_tokens_index_to_id(new_token_index, 0);
    assert_all_tokens_id_to_index(new_token_id, 0);

    state._add_token_to_all_tokens_enumeration(new_token_id);

    assert_all_tokens_index_to_id(new_token_index, new_token_id);
    assert_all_tokens_id_to_index(new_token_id, new_token_index);
    assert_total_supply(initial_supply + 1);
}

//
// Helpers
//

fn assert_total_supply(expected_supply: u32) {
    let state = @COMPONENT_STATE();
    assert_eq!(state.total_supply(), expected_supply.into());
}

fn assert_token_of_owner_by_index(owner: ContractAddress, expected_token_list: Span<u256>) {
    let state = @COMPONENT_STATE();
    let contract_state = @CONTRACT_STATE();

    // Check owner balance == expected_token_list
    let owner_bal = contract_state.balance_of(owner);
    let expected_list_len = expected_token_list.len().into();
    assert_eq!(owner_bal, expected_list_len);

    for i in 0..expected_token_list.len() {
        let token = state.token_of_owner_by_index(owner, i.into());
        assert_eq!(token, *expected_token_list.at(i));
    };
}

fn assert_token_by_index(expected_token_list: Span<u256>) {
    let state = @COMPONENT_STATE();

    // Check total_supply == expected_token_list
    let total_supply = state.total_supply();
    let expected_list_len = expected_token_list.len().into();
    assert_eq!(total_supply, expected_list_len);

    for i in 0..expected_token_list.len() {
        let token = state.token_by_index(i.into());
        assert_eq!(token, *expected_token_list.at(i));
    };
}

fn assert_all_tokens_index_to_id(index: u32, expected_token_id: u256) {
    let state = @COMPONENT_STATE();
    let index_to_id = state.ERC721Enumerable_all_tokens.read(index.into());
    assert_eq!(index_to_id, expected_token_id);
}

fn assert_all_tokens_id_to_index(token_id: u256, expected_index: u32) {
    let state = @COMPONENT_STATE();
    let id_to_index = state.ERC721Enumerable_all_tokens_index.read(token_id);
    assert_eq!(id_to_index, expected_index.into());
}

fn assert_owner_tokens_index_to_id(owner: ContractAddress, index: u32, expected_token_id: u256) {
    let state = @COMPONENT_STATE();
    let index_to_id = state.ERC721Enumerable_owned_tokens.read((owner, index.into()));
    assert_eq!(index_to_id, expected_token_id);
}

fn assert_owner_tokens_id_to_index(token_id: u256, expected_index: u32) {
    let state = @COMPONENT_STATE();
    let id_to_index = state.ERC721Enumerable_owned_tokens_index.read(token_id);
    assert_eq!(id_to_index, expected_index.into());
}

fn assert_all_tokens_of_owner(owner: ContractAddress, expected_tokens: Span<u256>) {
    let state = @COMPONENT_STATE();
    let tokens = state.all_tokens_of_owner(owner);
    assert_eq!(tokens, expected_tokens);
}

// Removes by swapping the token to remove with the last token to replicate
// ERC721Enumerable behaviour. Assumes that `tokens_list` contains no duplicates.
fn remove_token_from_list(tokens_list: Span<u256>, token_to_remove: u256) -> Span<u256> {
    let last_index = tokens_list.len() - 1;
    let mut index = 0;
    let mut is_found = false;
    let mut result = array![];
    while index <= last_index {
        let token = *tokens_list.at(index);
        if is_found {
            if index != last_index {
                result.append(token);
            }
        } else {
            if token == token_to_remove {
                is_found = true;
                if index != last_index {
                    let last_token = tokens_list.at(last_index);
                    result.append(*last_token);
                }
            } else {
                result.append(token);
            }
        }
        index += 1;
    }
    result.span()
}
