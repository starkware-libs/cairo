use starknet_types_core::felt::{Felt as Felt252, NonZeroFelt as NonZeroFelt252};
use starknet_types_core::hash::{Pedersen, StarkHash};

/// 2 ** 251 - 256
const ADDR_BOUND: NonZeroFelt252 =
    NonZeroFelt252::from_felt_unchecked(Felt252::from_hex_unchecked(
        "0x7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff00",
    ));

/// Cairo string of "STARKNET_CONTRACT_ADDRESS"
const CONTRACT_ADDRESS_PREFIX: Felt252 =
    Felt252::from_hex_unchecked("0x535441524b4e45545f434f4e54524143545f41444452455353");

/// Calculates the address of a Starknet contract, as defined in
/// <https://docs.starknet.io/documentation/architecture_and_concepts/Smart_Contracts/contract-address/>.
pub fn calculate_contract_address(
    salt: &Felt252,
    class_hash: &Felt252,
    constructor_calldata: &[Felt252],
    deployer_address: &Felt252,
) -> Felt252 {
    let constructor_calldata_hash = Pedersen::hash_array(constructor_calldata);
    Pedersen::hash_array(&[
        CONTRACT_ADDRESS_PREFIX,
        *deployer_address,
        *salt,
        *class_hash,
        constructor_calldata_hash,
    ])
    .mod_floor(&ADDR_BOUND)
}
