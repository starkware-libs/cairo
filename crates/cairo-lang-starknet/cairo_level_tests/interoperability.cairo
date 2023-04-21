use starknet::syscalls::deploy_syscall;
use array::ArrayTrait;
use traits::TryInto;
use option::OptionTrait;
use starknet::class_hash::Felt252TryIntoClassHash;
use debug::print_felt252;

#[contract]
mod ContractA {
    use debug::print_felt252;
    struct Storage {
        value: felt252, 
    }

    #[constructor]
    fn constructor(value_: felt252) {
        print_felt252(value_);
        value::write(value_);
    }
}

#[test]
#[available_gas(30000000)]
fn test_deploy() {
    let mut calldata = ArrayTrait::new();
    calldata.append(10);
    deploy_syscall(ContractA::TEST_CLASS_HASH.try_into().unwrap(), 0, calldata.span(), false);
    print_felt252(20);
}
