use array::ArrayTrait;
use serde::Serde;
use starknet::contract_address_to_felt;
use starknet::contract_address_try_from_felt;

impl ContractAddressSerde of Serde::<ContractAddress> {
    fn serialize(ref serialized: Array::<felt>, input: ContractAddress) {
        Serde::<felt>::serialize(ref serialized, contract_address_to_felt(input));
    }
    fn deserialize(ref serialized: Array::<felt>) -> Option::<ContractAddress> {
        Option::Some(contract_address_try_from_felt(Serde::<felt>::deserialize(ref serialized)?)?)
    }
}
