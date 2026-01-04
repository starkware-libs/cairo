use openzeppelin_testing as utils;
use openzeppelin_testing::constants;
use openzeppelin_token::erc20::interface::IERC20Dispatcher;
use openzeppelin_utils::serde::SerializedAppend;
use starknet::{ContractAddress, SyscallResultTrait};
use crate::vesting::interface::IVestingDispatcher;

#[derive(Copy, Drop)]
pub(crate) enum VestingStrategy {
    Linear,
    Steps: u64,
}

#[derive(Copy, Drop)]
pub(crate) struct TestData {
    pub strategy: VestingStrategy,
    pub total_allocation: u256,
    pub beneficiary: ContractAddress,
    pub start: u64,
    pub duration: u64,
    pub cliff_duration: u64,
}

fn deploy_vesting_mock(data: TestData) -> IVestingDispatcher {
    let contract_address = match data.strategy {
        VestingStrategy::Linear => {
            let mut calldata = array![];
            calldata.append_serde(data.beneficiary);
            calldata.append_serde(data.start);
            calldata.append_serde(data.duration);
            calldata.append_serde(data.cliff_duration);
            utils::declare_and_deploy("LinearVestingMock", calldata)
        },
        VestingStrategy::Steps(total_steps) => {
            let mut calldata = array![];
            calldata.append_serde(total_steps);
            calldata.append_serde(data.beneficiary);
            calldata.append_serde(data.start);
            calldata.append_serde(data.duration);
            calldata.append_serde(data.cliff_duration);
            utils::declare_and_deploy("StepsVestingMock", calldata)
        },
    };
    IVestingDispatcher { contract_address }
}

fn deploy_erc20_mock(recipient: ContractAddress, initial_supply: u256) -> IERC20Dispatcher {
    let mut calldata = array![];
    calldata.append_serde(constants::NAME());
    calldata.append_serde(constants::SYMBOL());
    calldata.append_serde(initial_supply);
    calldata.append_serde(recipient);

    let contract_address = utils::declare_and_deploy("ERC20OptionalTransferPanicMock", calldata);
    IERC20Dispatcher { contract_address }
}

pub(crate) fn setup(data: TestData) -> (IVestingDispatcher, ContractAddress) {
    let vesting = deploy_vesting_mock(data);
    let token = deploy_erc20_mock(vesting.contract_address, data.total_allocation);
    (vesting, token.contract_address)
}

pub(crate) fn set_transfer_to_fail(token: ContractAddress, should_fail: bool) {
    let mut calldata = array![];
    calldata.append_serde(true);
    starknet::syscalls::call_contract_syscall(
        token, selector!("set_transfer_should_fail"), calldata.span(),
    )
        .unwrap_syscall();
}
