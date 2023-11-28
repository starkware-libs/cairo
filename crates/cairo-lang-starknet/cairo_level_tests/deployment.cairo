use core::result::ResultTrait;
use starknet::deploy_syscall;

#[starknet::interface]
trait IFactory<TContractState> {
    fn get_value(self: @TContractState) -> felt252;
    fn set_value(ref self: TContractState, value: felt252);
}

#[starknet::interface]
trait IEmptyContract<TContractState> {
    fn initialize(ref self: TContractState);
}

#[starknet::contract]
mod Factory {
    use core::traits::TryInto;
    use starknet::{ClassHash, deploy_syscall};
    use super::{IEmptyContractDispatcher, IEmptyContractDispatcherTrait};
    #[storage]
    struct Storage {
        value: felt252,
    }

    #[constructor]
    fn constructor(ref self: ContractState, child_hash: ClassHash) {
        let calldata = array![];
        let (address, _) = deploy_syscall(child_hash.try_into().unwrap(), 0, calldata.span(), false)
            .expect('child deployment failed');
        IEmptyContractDispatcher { contract_address: address }.initialize();
    }

    #[abi(embed_v0)]
    impl FactoryImpl of super::IFactory<ContractState> {
        fn get_value(self: @ContractState) -> felt252 {
            self.value.read()
        }

        fn set_value(ref self: ContractState, value: felt252) {
            self.value.write(1);
        }
    }
}

#[starknet::contract]
mod EmptyContract {
    use starknet::account::{Call, AccountContract};
    use starknet::get_caller_address;
    use super::{IFactoryDispatcher, IFactoryDispatcherTrait};

    #[storage]
    struct Storage {}

    #[abi(embed_v0)]
    impl EmptyContractImpl of super::IEmptyContract<ContractState> {
        fn initialize(ref self: ContractState) {
            let deployer_address = get_caller_address();
            let factory = IFactoryDispatcher { contract_address: deployer_address };
            factory.set_value(1);
            assert(factory.get_value() == 1, 'factory value is not 1');
        }
    }
}

#[test]
fn test_deploy_in_construct() {
    let calldata = array![EmptyContract::TEST_CLASS_HASH.try_into().unwrap()];
    let (starknet_address, _) = deploy_syscall(
        Factory::TEST_CLASS_HASH.try_into().unwrap(), 0, calldata.span(), false
    )
        .expect('factory deployment failed');
    let res = IFactoryDispatcher { contract_address: starknet_address }.get_value();
    assert(res == 1, 'factory value is not 1')
}

