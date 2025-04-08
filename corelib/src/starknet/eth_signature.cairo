//! Utilities for Ethereum signature verification and address recovery.
//!
//! This module provides functionality for working with Ethereum signatures.
//! It implements verification of Ethereum signatures against addresses and conversion of public
//! keys to Ethereum addresses.

use core::keccak::keccak_u256s_be_inputs;
use core::option::OptionTrait;
#[allow(unused_imports)]
use starknet::secp256_trait::{
    Secp256PointTrait, Secp256Trait, Signature, is_signature_entry_valid, recover_public_key,
};
#[allow(unused_imports)]
use starknet::secp256k1::Secp256k1Point;
#[allow(unused_imports)]
use starknet::{EthAddress, SyscallResult, SyscallResultTrait};

/// Asserts that an Ethereum signature is valid for a given message hash and Ethereum address.
/// Also verifies that the `r` and `s` components of the signature are in the range `[1, N)`,
/// where N is the size of the curve.
///
/// # Arguments
///
/// * `msg_hash` - The 32-byte hash of the message that was signed
/// * `signature` - The Ethereum signature containing `r`, `s` components and `y_parity`
/// * `eth_address` - The expected Ethereum address of the signer
///
/// # Panics
///
/// Panics if:
/// * The signature components are out of range (not in [1, N) where N is the curve order)
/// * The recovered address doesn't match the provided address
///
/// # Examples
///
/// ```
/// use starknet::eth_address::EthAddress;
/// use starknet::eth_signature::verify_eth_signature;
/// use starknet::secp256_trait::Signature;
///
/// let msg_hash = 0xe888fbb4cf9ae6254f19ba12e6d9af54788f195a6f509ca3e934f78d7a71dd85;
/// let r = 0x4c8e4fbc1fbb1dece52185e532812c4f7a5f81cf3ee10044320a0d03b62d3e9a;
/// let s = 0x4ac5e5c0c0e8a4871583cc131f35fb49c2b7f60e6a8b84965830658f08f7410c;
/// let y_parity = true;
/// let eth_address: EthAddress = 0x767410c1bb448978bd42b984d7de5970bcaf5c43_u256
///     .try_into()
///     .unwrap();
/// verify_eth_signature(msg_hash, Signature { r, s, y_parity }, eth_address);
/// ```
pub fn verify_eth_signature(msg_hash: u256, signature: Signature, eth_address: EthAddress) {
    match is_eth_signature_valid(:msg_hash, :signature, :eth_address) {
        Ok(()) => {},
        Err(err) => core::panic_with_felt252(err),
    }
}

/// Validates an Ethereum signature against a message hash and Ethereum address.
/// Similar to `verify_eth_signature` but returns a `Result` instead of panicking.
/// Also verifies that `r` and `s` components of the signature are in the range `[1, N)`,
/// where N is the size of the curve.
///
/// # Arguments
///
/// * `msg_hash` - The 32-byte hash of the message that was signed
/// * `signature` - The Ethereum signature containing `r`, `s` components and `y_parity`
/// * `eth_address` - The expected Ethereum address of the signer
///
/// # Returns
///
/// Returns `Ok(())` if the signature is valid, or `Err(felt252)` containing an error message if
/// invalid.
///
/// # Examples
///
/// ```
/// use starknet::eth_address::EthAddress;
/// use starknet::eth_signature::is_eth_signature_valid;
/// use starknet::secp256_trait::Signature;
///
/// let msg_hash = 0xe888fbb4cf9ae6254f19ba12e6d9af54788f195a6f509ca3e934f78d7a71dd85;
/// let r = 0x4c8e4fbc1fbb1dece52185e532812c4f7a5f81cf3ee10044320a0d03b62d3e9a;
/// let s = 0x4ac5e5c0c0e8a4871583cc131f35fb49c2b7f60e6a8b84965830658f08f7410c;
/// let y_parity = true;
/// let eth_address: EthAddress = 0x767410c1bb448978bd42b984d7de5970bcaf5c43_u256
///     .try_into()
///     .unwrap();
/// assert!(is_eth_signature_valid(msg_hash, Signature { r, s, y_parity }, eth_address).is_ok());
/// ```
pub fn is_eth_signature_valid(
    msg_hash: u256, signature: Signature, eth_address: EthAddress,
) -> Result<(), felt252> {
    if !is_signature_entry_valid::<Secp256k1Point>(signature.r) {
        return Err('Signature out of range');
    }
    if !is_signature_entry_valid::<Secp256k1Point>(signature.s) {
        return Err('Signature out of range');
    }

    let public_key_point = recover_public_key::<Secp256k1Point>(:msg_hash, :signature).unwrap();
    let calculated_eth_address = public_key_point_to_eth_address(:public_key_point);
    if eth_address != calculated_eth_address {
        return Err('Invalid signature');
    }
    Ok(())
}

/// Converts a public key point to its corresponding Ethereum address.
///
/// The Ethereum address is calculated by taking the Keccak-256 hash of the public key coordinates
/// and taking the last 20 big-endian bytes.
///
/// # Arguments
///
/// * `public_key_point` - A point on a secp256 curve representing a public key
///
/// # Returns
///
/// The 20-byte Ethereum address derived from the public key
///
/// # Examples
///
/// ```
/// use starknet::eth_signature::public_key_point_to_eth_address;
/// use starknet::secp256k1::Secp256k1Point;
/// use starknet::secp256_trait::Secp256Trait;
///
/// let public_key: Secp256k1Point = Secp256Trait::secp256_ec_get_point_from_x_syscall(
///     0xa9a02d48081294b9bb0d8740d70d3607feb20876964d432846d9b9100b91eefd, false,
/// )
///     .unwrap()
///     .unwrap();
/// let eth_address = public_key_point_to_eth_address(public_key);
/// assert!(eth_address == 0x767410c1bb448978bd42b984d7de5970bcaf5c43.try_into().unwrap());
/// ```
pub fn public_key_point_to_eth_address<
    Secp256Point,
    +Drop<Secp256Point>,
    +Secp256Trait<Secp256Point>,
    +Secp256PointTrait<Secp256Point>,
>(
    public_key_point: Secp256Point,
) -> EthAddress {
    let (x, y) = public_key_point.get_coordinates().unwrap_syscall();

    // Keccak output is little endian.
    let point_hash_le = keccak_u256s_be_inputs([x, y].span());
    let point_hash = u256 {
        low: core::integer::u128_byte_reverse(point_hash_le.high),
        high: core::integer::u128_byte_reverse(point_hash_le.low),
    };

    point_hash.into()
}
