use starknet::{account::Call, ContractAddress};
use core::test::test_utils::{assert_eq, assert_ne};

use super::utils::serialized;

#[starknet::interface]
trait IAnotherContract<T> {}

#[starknet::contract(account)]
mod test_contract {
    use starknet::{account::Call, ContractAddress, ClassHash};
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

    #[external(v0)]
    fn __validate__(ref self: ContractState, calls: Array<Call>) {}
    #[external(v0)]
    fn __execute__(ref self: ContractState, mut calls: Array<Call>) -> Array<Span<felt252>> {
        array![]
    }
}

#[test]
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
fn test_library_dispatcher_serialization() {
    let a = starknet::contract_address_const::<11>();
    test_contract::__external::set_another_class_hash(serialized(a));
    assert_eq(
        @test_contract::__external::get_another_class_hash(serialized(())),
        @serialized(a),
        'Wrong result'
    );
}


// Calls `withdraw_gas` the than return the available gas.
// This is useful in test as the `withdraw_gas` allows the gas wallet to be ~0 at the call site.
// Note that this function must be `inline(always)`.
#[inline(always)]
pub fn withdraw_and_get_available_gas() -> u128 {
    core::gas::withdraw_gas().unwrap();
    core::testing::get_available_gas()
}


// Tests the serialization and deserialize of the arguments to `__validate__`.
#[test]
fn test_validate_gas_cost() {
    let contract_address = starknet::contract_address_const::<11>();
    let base_gas = withdraw_and_get_available_gas();
    let calls = array![
        Call {
            to: contract_address,
            selector: 0x219209e083275171774dab1df80982e9df2096516f06319c5c6d71ae0a8480c,
            calldata: array![
                0x7a6f98c03379b9513ca84cca1373ff452a7462a3b61598f0af5bb27ad7f76d1, 0x4db5d32, 0x0
            ]
                .span()
        },
        Call {
            to: contract_address,
            selector: 0x2c0f7bf2d6cf5304c29171bf493feb222fef84bdaf17805a6574b0c2e8bcc87,
            calldata: array![
                0x4db5d32,
                0x0,
                0x896ba264a31df2,
                0x0,
                0x2,
                0x53c91253bc9682c04929ca02ed00b3e423f6710d2ee7e0d5ebb06f3ecf368a8,
                0x49d36570d4e46f48e99674bd3fcc84644ddd6b96f7c741b1562b82f9e004dc7,
                0x54767f773cc172172c3afc5265bd0a76089c24cdef409635d27ac1a1fa96ca8,
                0x65586264
            ]
                .span()
        },
    ];
    let post_call_building_gas = withdraw_and_get_available_gas();

    let serialized_args = serialized(calls);
    let post_serialization_gas = withdraw_and_get_available_gas();

    test_contract::__wrapper____validate__(serialized_args);
    let post_call_gas = withdraw_and_get_available_gas();

    let call_building_gas_usage = base_gas - post_call_building_gas;
    let serialization_gas_usage = post_call_building_gas - post_serialization_gas;
    let entry_point_gas_usage = post_serialization_gas - post_call_gas;
    assert!(
        call_building_gas_usage == 3150
            && serialization_gas_usage == 50950
            && entry_point_gas_usage == 141400,
        "Unexpected gas_usage:
     call_building: `{call_building_gas_usage}`.
     serialization: `{serialization_gas_usage}`.
     entry_point: `{entry_point_gas_usage}`."
    );
}
