use array::ArrayTrait;
use array::SpanTrait;
use starknet::ContractAddress;
use test::test_utils::{assert_eq, assert_ne};

use super::utils::serialized_element;
use super::utils::single_deserialize;

#[starknet::interface]
trait IAnotherContract<T> {}

#[starknet::contract]
mod TestContract {
    use starknet::{ContractAddress, ClassHash};
    use super::{
        IAnotherContractDispatcher, IAnotherContractLibraryDispatcher,
        IAnotherContractDispatcherTrait
    };


    #[storage]
    struct Storage {
        another: IAnotherContractDispatcher,
        another_as_library: IAnotherContractLibraryDispatcher
    }

    #[external]
    fn get_another_address(self: @ContractState) -> ContractAddress {
        self.another.read().contract_address
    }

    #[external]
    fn set_another_address(ref self: ContractState, contract_address: ContractAddress) {
        self.another.write(IAnotherContractDispatcher { contract_address });
    }

    #[external]
    fn get_another_class_hash(self: @ContractState) -> ClassHash {
        self.another_as_library.read().class_hash
    }

    #[external]
    fn set_another_class_hash(ref self: ContractState, class_hash: ClassHash) {
        self.another_as_library.write(IAnotherContractLibraryDispatcher { class_hash });
    }
}

#[test]
#[available_gas(70000)]
fn test_dispatcher_serialization() {
    let a = starknet::contract_address_const::<11>();
    TestContract::__external::set_another_address(serialized_element(a));
    let mut retdata = TestContract::__external::get_another_address(ArrayTrait::new().span());
    assert_eq(@single_deserialize(ref retdata), @a, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(70000)]
fn test_library_dispatcher_serialization() {
    let a = starknet::contract_address_const::<11>();
    TestContract::__external::set_another_class_hash(serialized_element(a));
    let mut retdata = TestContract::__external::get_another_class_hash(ArrayTrait::new().span());
    assert_eq(@single_deserialize(ref retdata), @a, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}
