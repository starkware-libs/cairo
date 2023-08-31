use starknet::ContractAddress;
use test::test_utils::{assert_eq, assert_ne};

use super::utils::serialized;

#[starknet::interface]
trait IAnotherContract<T> {}

#[starknet::contract]
mod test_contract {
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

    #[external(v0)]
    fn get_another_address(self: @ContractState) -> ContractAddress {
        self.another.read().contract_address
    }

    #[external(v0)]
    fn set_another_address(ref self: ContractState, contract_address: ContractAddress) {
        self.another.write(IAnotherContractDispatcher { contract_address });
    }

    #[external(v0)]
    fn get_another_class_hash(self: @ContractState) -> ClassHash {
        self.another_as_library.read().class_hash
    }

    #[external(v0)]
    fn set_another_class_hash(ref self: ContractState, class_hash: ClassHash) {
        self.another_as_library.write(IAnotherContractLibraryDispatcher { class_hash });
    }
}

#[test]
#[available_gas(100000)]
fn test_dispatcher_serialization() {
    let a = starknet::contract_address_const::<11>();
    test_contract::__external::set_another_address(serialized(a));
    assert_eq(
        @test_contract::__external::get_another_address(serialized(())),
        @serialized(a),
        'Wrong result'
    );
}

#[test]
#[available_gas(100000)]
fn test_library_dispatcher_serialization() {
    let a = starknet::contract_address_const::<11>();
    test_contract::__external::set_another_class_hash(serialized(a));
    assert_eq(
        @test_contract::__external::get_another_class_hash(serialized(())),
        @serialized(a),
        'Wrong result'
    );
}
