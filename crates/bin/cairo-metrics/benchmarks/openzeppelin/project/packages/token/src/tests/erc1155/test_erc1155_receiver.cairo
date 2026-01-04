use openzeppelin_introspection::interface::ISRC5_ID;
use openzeppelin_introspection::src5::SRC5Component::SRC5Impl;
use openzeppelin_test_common::mocks::erc1155::DualCaseERC1155ReceiverMock;
use openzeppelin_testing::constants::{EMPTY_DATA, OPERATOR, OWNER, TOKEN_ID, TOKEN_VALUE};
use crate::erc1155::ERC1155ReceiverComponent::{
    ERC1155ReceiverCamelImpl, ERC1155ReceiverImpl, InternalImpl,
};
use crate::erc1155::interface::IERC1155_RECEIVER_ID;

fn STATE() -> DualCaseERC1155ReceiverMock::ContractState {
    DualCaseERC1155ReceiverMock::contract_state_for_testing()
}

#[test]
fn test_initializer() {
    let mut state = STATE();
    state.erc1155_receiver.initializer();

    let supports_ierc1155_receiver = state.src5.supports_interface(IERC1155_RECEIVER_ID);
    assert!(supports_ierc1155_receiver);

    let supports_isrc5 = state.src5.supports_interface(ISRC5_ID);
    assert!(supports_isrc5);
}

//
// on_erc1155_received & onERC1155Received
//

#[test]
fn test_on_erc1155_received() {
    let mut state = STATE();
    let on_erc1155_received = state
        .erc1155_receiver
        .on_erc1155_received(OPERATOR, OWNER, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
    assert_eq!(on_erc1155_received, IERC1155_RECEIVER_ID);
}

#[test]
fn test_onERC1155Received() {
    let mut state = STATE();
    let on_erc1155_received = state
        .erc1155_receiver
        .onERC1155Received(OPERATOR, OWNER, TOKEN_ID, TOKEN_VALUE, EMPTY_DATA());
    assert_eq!(on_erc1155_received, IERC1155_RECEIVER_ID);
}

//
// on_erc1155_batch_received & onERC1155BatchReceived
//

#[test]
fn test_on_erc1155_batch_received() {
    let mut state = STATE();
    let (token_ids, values) = get_ids_and_values();
    let on_erc1155_received = state
        .erc1155_receiver
        .on_erc1155_batch_received(OPERATOR, OWNER, token_ids, values, EMPTY_DATA());
    assert_eq!(on_erc1155_received, IERC1155_RECEIVER_ID);
}

#[test]
fn test_onERC1155BatchReceived() {
    let mut state = STATE();
    let (token_ids, values) = get_ids_and_values();
    let on_erc1155_received = state
        .erc1155_receiver
        .onERC1155BatchReceived(OPERATOR, OWNER, token_ids, values, EMPTY_DATA());
    assert_eq!(on_erc1155_received, IERC1155_RECEIVER_ID);
}

//
// Helpers
//

fn get_ids_and_values() -> (Span<u256>, Span<u256>) {
    let token_ids = array![TOKEN_ID, TOKEN_ID].span();
    let values = array![TOKEN_VALUE, TOKEN_VALUE].span();
    (token_ids, values)
}
