//! Utilities for Ethereum signatures on Starknet.
//!
//! This module provides functions that allow for:
//! - Verification that a given Ethereum signature is valid.
//! - Converting a public key point to the corresponding Ethereum address.

use core::keccak::keccak_u256s_be_inputs;
use core::option::OptionTrait;
#[allow(unused_imports)]
use starknet::{
    EthAddress,
    secp256_trait::{
        Secp256Trait, Secp256PointTrait, recover_public_key, is_signature_entry_valid, Signature,
    },
    secp256k1::Secp256k1Point, SyscallResult, SyscallResultTrait,
};

/// Asserts that an Ethereum signature is valid with respect to a given Ethereum address.
/// Also verifies that `r` and `s` components of the signature are in the range `(0, N)`,
/// where N is the size of the curve.
///
/// # Panics
///
/// Panics it the signature is incorrect.
///
/// # Examples
///
/// ```
/// use core::starknet::eth_address::EthAddress;
/// use core::starknet::eth_signature::verify_eth_signature;
/// use core::starknet::secp256_trait::Signature;
///
/// let signer: EthAddress = 'ethereum address'.try_into().unwrap();
/// let signature = Signature { r: 'r component'.into(), s: 's component'.into(), y_parity: true };
/// let message_hash: u256 = 'message hash'.into();
///
/// verify_eth_signature(message_hash, signature, signer);
/// ```
pub fn verify_eth_signature(msg_hash: u256, signature: Signature, eth_address: EthAddress) {
    match is_eth_signature_valid(:msg_hash, :signature, :eth_address) {
        Result::Ok(()) => {},
        Result::Err(err) => core::panic_with_felt252(err),
    }
}

/// Asserts that an Ethereum signature is valid with respect to a given Ethereum address.
/// Also verifies that `r` and `s` components of the signature are in the range `(0, N)`,
/// where N is the size of the curve.
///
/// Returns a `Result` with an error string if the signature is invalid.
///
/// # Examples
///
/// ```
/// use core::starknet::eth_address::EthAddress;
/// use core::starknet::eth_signature::is_eth_signature_valid;
/// use core::starknet::secp256_trait::Signature;
///
/// let signer: EthAddress = 'ethereum address'.try_into().unwrap();
/// let signature = Signature { r: 'r component'.into(), s: 's component'.into(), y_parity: true };
/// let message_hash: u256 = 'message hash'.into();
///
/// assert!(is_eth_signature_valid(message_hash, signature, signer) == Result::Ok(()));
/// ```
pub fn is_eth_signature_valid(
    msg_hash: u256, signature: Signature, eth_address: EthAddress,
) -> Result<(), felt252> {
    if !is_signature_entry_valid::<Secp256k1Point>(signature.r) {
        return Result::Err('Signature out of range');
    }
    if !is_signature_entry_valid::<Secp256k1Point>(signature.s) {
        return Result::Err('Signature out of range');
    }

    let public_key_point = recover_public_key::<Secp256k1Point>(:msg_hash, :signature).unwrap();
    let calculated_eth_address = public_key_point_to_eth_address(:public_key_point);
    if eth_address != calculated_eth_address {
        return Result::Err('Invalid signature');
    }
    Result::Ok(())
}

/// Converts a public key point to the corresponding Ethereum address.
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
