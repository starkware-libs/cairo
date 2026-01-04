use openzeppelin_access::ownable::OwnableComponent;
use openzeppelin_access::ownable::OwnableComponent::OwnershipTransferred;
use openzeppelin_testing::{EventSpyExt, EventSpyQueue as EventSpy};
use starknet::ContractAddress;

#[generate_trait]
pub impl OwnableSpyHelpersImpl of OwnableSpyHelpers {
    fn assert_only_event_ownership_transferred(
        ref self: EventSpy,
        contract: ContractAddress,
        previous_owner: ContractAddress,
        new_owner: ContractAddress,
    ) {
        self.assert_event_ownership_transferred(contract, previous_owner, new_owner);
        self.assert_no_events_left_from(contract);
    }

    fn assert_event_ownership_transferred(
        ref self: EventSpy,
        contract: ContractAddress,
        previous_owner: ContractAddress,
        new_owner: ContractAddress,
    ) {
        let expected = OwnableComponent::Event::OwnershipTransferred(
            OwnershipTransferred { previous_owner, new_owner },
        );
        self.assert_emitted_single(contract, expected);
    }
}
