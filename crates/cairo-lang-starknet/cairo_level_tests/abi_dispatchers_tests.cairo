use array::ArrayTrait;
use array::SpanTrait;
use starknet::ContractAddress;
use test::test_utils::{assert_eq, assert_ne};

use super::utils::serialized_element;
use super::utils::single_deserialize;

#[abi]
trait IAnotherContract {}

#[contract]
mod TestContract {
    use starknet::ContractAddress;
    use starknet::ClassHash;
    use super::IAnotherContractDispatcher;
    use super::IAnotherContractLibraryDispatcher;
    use super::IAnotherContractDispatcherTrait;


    struct Storage {
        another: IAnotherContractDispatcher,
        another_as_library: IAnotherContractLibraryDispatcher
    }

    #[view]
    fn get_another_address() -> ContractAddress {
        another::read().contract_address
    }

    #[external]
    fn set_another_address(contract_address: ContractAddress) {
        another::write(IAnotherContractDispatcher { contract_address });
    }

    #[view]
    fn get_another_class_hash() -> ClassHash {
        another_as_library::read().class_hash
    }

    #[external]
    fn set_another_class_hash(class_hash: ClassHash) {
        another_as_library::write(IAnotherContractLibraryDispatcher { class_hash });
    }
}

#[test]
#[available_gas(70000)]
fn test_dispatcher_serialization() {
    let a = starknet::contract_address_const::<11>();
    TestContract::__external::set_another_address(serialized_element(a));
    let mut retdata = TestContract::__external::get_another_address(ArrayTrait::new().span());
    assert_eq(single_deserialize(ref retdata), a, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}

#[test]
#[available_gas(70000)]
fn test_library_dispatcher_serialization() {
    let a = starknet::contract_address_const::<11>();
    TestContract::__external::set_another_class_hash(serialized_element(a));
    let mut retdata = TestContract::__external::get_another_class_hash(ArrayTrait::new().span());
    assert_eq(single_deserialize(ref retdata), a, 'Wrong result');
    assert(retdata.is_empty(), 'Array not empty');
}
