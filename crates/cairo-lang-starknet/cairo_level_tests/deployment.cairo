use starknet::syscalls::deploy_syscall;

#[starknet::interface]
trait IValue<TContractState> {
    fn get_value(self: @TContractState) -> felt252;
    fn set_value(ref self: TContractState, value: felt252);
}

#[starknet::contract]
mod self_caller {
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use super::{IValueDispatcher, IValueDispatcherTrait};
    #[storage]
    struct Storage {
        value: felt252,
    }

    #[constructor]
    fn constructor(ref self: ContractState) {
        IValueDispatcher { contract_address: starknet::get_contract_address() }.set_value(1);
    }

    #[abi(embed_v0)]
    impl ValueImpl of super::IValue<ContractState> {
        fn get_value(self: @ContractState) -> felt252 {
            self.value.read()
        }

        fn set_value(ref self: ContractState, value: felt252) {
            self.value.write(value);
        }
    }
}

#[test]
fn test_deploy_in_construct() {
    let (contract_address, _) = deploy_syscall(self_caller::TEST_CLASS_HASH, 0, [].span(), false)
        .expect('deployment failed');
    assert!(IValueDispatcher { contract_address }.get_value() == 1);
}


#[test]
fn test_redeploy_in_construct() {
    assert!(deploy_syscall(self_caller::TEST_CLASS_HASH, 0, [].span(), false).is_ok());
    assert!(
        deploy_syscall(
            self_caller::TEST_CLASS_HASH, 0, [].span(), false,
        ) == Err(array!['CONTRACT_ALREADY_DEPLOYED']),
    );
}
