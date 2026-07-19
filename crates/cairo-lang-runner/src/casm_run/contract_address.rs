use starknet_types_core::felt::{Felt as Felt252, NonZeroFelt as NonZeroFelt252};
use starknet_types_core::hash::{Blake2Felt252, Pedersen, StarkHash};

/// 2 ** 251 - 256
const ADDR_BOUND_FELT: Felt252 = Felt252::from_hex_unchecked(
    "0x7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff00",
);

/// 2 ** 251 - 256
const ADDR_BOUND: NonZeroFelt252 = NonZeroFelt252::from_felt_unchecked(ADDR_BOUND_FELT);

/// The field prime minus [`ADDR_BOUND_FELT`]. Addresses below this bound have a second lift into
/// the field: both `address` and `address + ADDR_BOUND_FELT` are below the prime.
const SECOND_LIFT_BOUND: Felt252 =
    Felt252::from_hex_unchecked("0x11000000000000000000000000000000000000000000000101");

/// The STARK curve is `y^2 = x^3 + ALPHA * x + BETA`.
const STARK_CURVE_ALPHA: Felt252 = Felt252::ONE;
const STARK_CURVE_BETA: Felt252 = Felt252::from_hex_unchecked(
    "0x6f21413efbe40de150e596d72f7a8c5609ad26c15c915c1f4cdfcb99cee9e89",
);

/// Cairo string of "STARKNET_CONTRACT_ADDRESS".
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

/// Calculates the address of a Starknet contract with the Blake-escaped derivation used by the
/// `deploy_v2` syscall (and `deploy_account` v4).
///
/// The raw address is the BLAKE2s felt-array hash of the same preimage `deploy` hashes with
/// Pedersen, reduced modulo [`ADDR_BOUND_FELT`]. It is then incremented (wrapping, skipping the
/// reserved `0x0`/`0x1`) until it is provably unreachable by any Pedersen derivation, so the two
/// schemes' address spaces are disjoint. This must match the sequencer's
/// `starknet_api::core::calculate_contract_address` for the `Blake2` arm.
pub fn calculate_contract_address_blake_escaped(
    salt: &Felt252,
    class_hash: &Felt252,
    constructor_calldata: &[Felt252],
    deployer_address: &Felt252,
) -> Felt252 {
    let constructor_calldata_hash =
        Blake2Felt252::encode_felt252_data_and_calc_blake_hash(constructor_calldata);
    let raw_address = Blake2Felt252::encode_felt252_data_and_calc_blake_hash(&[
        CONTRACT_ADDRESS_PREFIX,
        *deployer_address,
        *salt,
        *class_hash,
        constructor_calldata_hash,
    ])
    .mod_floor(&ADDR_BOUND);
    escape_pedersen_image(raw_address)
}

/// Returns `value^3 + STARK_CURVE_ALPHA * value + STARK_CURVE_BETA` — a square in the field iff
/// `value` is the x-coordinate of a STARK curve point.
fn stark_curve_cubic(value: &Felt252) -> Felt252 {
    value * value * value + STARK_CURVE_ALPHA * value + STARK_CURVE_BETA
}

/// Returns whether `value` is the x-coordinate of a STARK curve point (`stark_curve_cubic(value)`
/// is a quadratic residue, i.e. `t == 0` or `legendre(t) == 1`).
fn is_stark_curve_x_coordinate(value: &Felt252) -> bool {
    stark_curve_cubic(value).sqrt().is_some()
}

/// Returns whether some Pedersen hash output reduces (mod [`ADDR_BOUND_FELT`]) to `address`. A
/// Pedersen output is the x-coordinate of a STARK curve point, so `address` is reachable iff one of
/// its lifts into the field is a curve x-coordinate.
pub fn is_pedersen_reachable_address(address: &Felt252) -> bool {
    is_stark_curve_x_coordinate(address)
        || (*address < SECOND_LIFT_BOUND
            && is_stark_curve_x_coordinate(&(address + ADDR_BOUND_FELT)))
}

/// Increments `raw_address` (wrapping mod [`ADDR_BOUND_FELT`], skipping the reserved `0x0`/`0x1`)
/// until no Pedersen derivation can reach it. Expected ~1 increment; each step costs one
/// residuosity check, never a re-hash.
fn escape_pedersen_image(raw_address: Felt252) -> Felt252 {
    let mut address = raw_address;
    while address < Felt252::TWO || is_pedersen_reachable_address(&address) {
        address += Felt252::ONE;
        if address == ADDR_BOUND_FELT {
            address = Felt252::ZERO;
        }
    }
    address
}
