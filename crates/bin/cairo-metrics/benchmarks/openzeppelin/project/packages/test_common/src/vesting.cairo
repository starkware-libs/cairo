use openzeppelin_finance::vesting::VestingComponent;
use openzeppelin_finance::vesting::VestingComponent::AmountReleased;
use openzeppelin_testing::{EventSpyExt, EventSpyQueue as EventSpy};
use starknet::ContractAddress;

#[generate_trait]
pub impl VestingSpyHelpersImpl of VestingSpyHelpers {
    fn assert_only_event_amount_released(
        ref self: EventSpy, contract: ContractAddress, token: ContractAddress, amount: u256,
    ) {
        let expected = VestingComponent::Event::AmountReleased(AmountReleased { token, amount });
        self.assert_only_event(contract, expected);
    }
}
