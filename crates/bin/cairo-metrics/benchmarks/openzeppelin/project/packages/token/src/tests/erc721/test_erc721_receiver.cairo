use openzeppelin_introspection::interface::ISRC5_ID;
use openzeppelin_introspection::src5::SRC5Component::SRC5Impl;
use openzeppelin_test_common::mocks::erc721::DualCaseERC721ReceiverMock;
use openzeppelin_testing::constants::{OPERATOR, OWNER, TOKEN_ID};
use crate::erc721::ERC721ReceiverComponent::{
    ERC721ReceiverCamelImpl, ERC721ReceiverImpl, InternalImpl,
};
use crate::erc721::interface::IERC721_RECEIVER_ID;

fn STATE() -> DualCaseERC721ReceiverMock::ContractState {
    DualCaseERC721ReceiverMock::contract_state_for_testing()
}

#[test]
fn test_initializer() {
    let mut state = STATE();
    state.erc721_receiver.initializer();

    let supports_ierc721_receiver = state.src5.supports_interface(IERC721_RECEIVER_ID);
    assert!(supports_ierc721_receiver);

    let supports_isrc5 = state.src5.supports_interface(ISRC5_ID);
    assert!(supports_isrc5);
}

#[test]
fn test_on_erc721_received() {
    let mut state = STATE();
    let data = array![];

    let on_erc721_received = state
        .erc721_receiver
        .on_erc721_received(OPERATOR, OWNER, TOKEN_ID, data.span());
    assert_eq!(on_erc721_received, IERC721_RECEIVER_ID, "Should return receiver ID");

    let onERC721Received = state
        .erc721_receiver
        .onERC721Received(OPERATOR, OWNER, TOKEN_ID, data.span());
    assert_eq!(onERC721Received, IERC721_RECEIVER_ID, "Should return receiver ID");
}
