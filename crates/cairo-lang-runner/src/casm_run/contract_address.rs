use cairo_felt::Felt252;
use starknet_crypto::{pedersen_hash, FieldElement};

/// Computes Pedersen hash using STARK curve on an array of elements, as defined
/// in <https://docs.starknet.io/documentation/architecture_and_concepts/Hashing/hash-functions/#array_hashing.>
pub fn pedersen_hash_array(felts: &[FieldElement]) -> FieldElement {
    let current_hash = felts
        .iter()
        .fold(FieldElement::from(0_u8), |current_hash, felt| pedersen_hash(&current_hash, felt));
    let data_len =
        FieldElement::from(u128::try_from(felts.len()).expect("Got 2^128 felts or more."));
    pedersen_hash(&current_hash, &data_len)
}

/// 2 ** 251 - 256
const ADDR_BOUND: FieldElement = FieldElement::from_mont([
    18446743986131443745,
    160989183,
    18446744073709255680,
    576459263475590224,
]);

/// Cairo string of "STARKNET_CONTRACT_ADDRESS"
const CONTRACT_ADDRESS_PREFIX: FieldElement = FieldElement::from_mont([
    3829237882463328880,
    17289941567720117366,
    8635008616843941496,
    533439743893157637,
]);

/// Converts a Felt252 to the FieldElement type used in starknet-crypto.
fn felt252_to_field_element(input: &Felt252) -> FieldElement {
    FieldElement::from_bytes_be(&input.to_be_bytes()).unwrap()
}

/// Calculates the address of a starknet contract, as defined in
/// <https://docs.starknet.io/documentation/architecture_and_concepts/Smart_Contracts/contract-address/>.
pub fn calculate_contract_address(
    salt: &Felt252,
    class_hash: &Felt252,
    constructor_calldata: &[Felt252],
    deployer_address: &Felt252,
) -> Felt252 {
    let constructor_calldata_hash = pedersen_hash_array(
        &constructor_calldata.iter().map(felt252_to_field_element).collect::<Vec<_>>(),
    );
    let mut address = pedersen_hash_array(&[
        CONTRACT_ADDRESS_PREFIX,
        felt252_to_field_element(deployer_address),
        felt252_to_field_element(salt),
        felt252_to_field_element(class_hash),
        constructor_calldata_hash,
    ]);
    address = address % ADDR_BOUND;

    Felt252::from_bytes_be(&address.to_bytes_be())
}
