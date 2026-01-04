use openzeppelin_testing::{EventSpyExt, EventSpyQueue as EventSpy};
use openzeppelin_token::erc721::ERC721Component;
use openzeppelin_token::erc721::ERC721Component::{Approval, ApprovalForAll, Transfer};
use starknet::ContractAddress;

#[generate_trait]
pub impl ERC721SpyHelpersImpl of ERC721SpyHelpers {
    fn assert_event_approval_for_all(
        ref self: EventSpy,
        contract: ContractAddress,
        owner: ContractAddress,
        operator: ContractAddress,
        approved: bool,
    ) {
        let expected = ERC721Component::Event::ApprovalForAll(
            ApprovalForAll { owner, operator, approved },
        );
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_approval_for_all(
        ref self: EventSpy,
        contract: ContractAddress,
        owner: ContractAddress,
        operator: ContractAddress,
        approved: bool,
    ) {
        self.assert_event_approval_for_all(contract, owner, operator, approved);
        self.assert_no_events_left_from(contract);
    }

    fn assert_event_approval(
        ref self: EventSpy,
        contract: ContractAddress,
        owner: ContractAddress,
        approved: ContractAddress,
        token_id: u256,
    ) {
        let expected = ERC721Component::Event::Approval(Approval { owner, approved, token_id });
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_approval(
        ref self: EventSpy,
        contract: ContractAddress,
        owner: ContractAddress,
        approved: ContractAddress,
        token_id: u256,
    ) {
        self.assert_event_approval(contract, owner, approved, token_id);
        self.assert_no_events_left_from(contract);
    }

    fn assert_event_transfer(
        ref self: EventSpy,
        contract: ContractAddress,
        from: ContractAddress,
        to: ContractAddress,
        token_id: u256,
    ) {
        let expected = ERC721Component::Event::Transfer(Transfer { from, to, token_id });
        self.assert_emitted_single(contract, expected);
    }

    fn assert_only_event_transfer(
        ref self: EventSpy,
        contract: ContractAddress,
        from: ContractAddress,
        to: ContractAddress,
        token_id: u256,
    ) {
        self.assert_event_transfer(contract, from, to, token_id);
        self.assert_no_events_left_from(contract);
    }
}
