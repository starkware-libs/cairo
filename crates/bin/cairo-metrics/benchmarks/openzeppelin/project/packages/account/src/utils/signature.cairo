// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (account/src/utils/signature.cairo)

use core::ecdsa::check_ecdsa_signature;
use starknet::secp256_trait;
use starknet::secp256_trait::{Secp256PointTrait, Secp256Trait};
use crate::interface::{EthPublicKey, P256PublicKey};

/// A signature format compatible with the family of secp256 curves.
#[derive(Copy, Drop, Serde)]
pub struct Secp256Signature {
    pub r: u256,
    pub s: u256,
}

/// This function assumes the `s` component of the signature to be positive
/// for efficiency reasons. It is not recommended to use it other than for
/// validating account signatures over transaction hashes since otherwise
/// it's not protected against signature malleability.
/// See https://github.com/OpenZeppelin/cairo-contracts/issues/889.
pub fn is_valid_stark_signature(
    msg_hash: felt252, public_key: felt252, signature: Span<felt252>,
) -> bool {
    let valid_length = signature.len() == 2;

    if valid_length {
        check_ecdsa_signature(msg_hash, public_key, *signature.at(0_u32), *signature.at(1_u32))
    } else {
        false
    }
}

/// This function assumes the `s` component of the signature to be positive
/// for efficiency reasons. It is not recommended to use it other than for
/// validating account signatures over transaction hashes since otherwise
/// it's not protected against signature malleability.
/// See https://github.com/OpenZeppelin/cairo-contracts/issues/889.
pub fn is_valid_eth_signature(
    msg_hash: felt252, public_key: EthPublicKey, signature: Span<felt252>,
) -> bool {
    is_valid_secp256_signature(msg_hash, public_key, signature)
}

/// This function assumes the `s` component of the signature to be positive
/// for efficiency reasons. It is not recommended to use it other than for
/// validating account signatures over transaction hashes since otherwise
/// it's not protected against signature malleability.
/// See https://github.com/OpenZeppelin/cairo-contracts/issues/889.
pub fn is_valid_p256_signature(
    msg_hash: felt252, public_key: P256PublicKey, signature: Span<felt252>,
) -> bool {
    is_valid_secp256_signature(msg_hash, public_key, signature)
}

/// Verifies a Secp256 compatible signature for a valid point in the corresponding curve.
fn is_valid_secp256_signature<
    Secp256Point,
    +Secp256Trait<Secp256Point>,
    +Secp256PointTrait<Secp256Point>,
    +Drop<Secp256Point>,
>(
    msg_hash: felt252, public_key: Secp256Point, signature: Span<felt252>,
) -> bool {
    let mut signature = signature;
    let signature: Secp256Signature = Serde::deserialize(ref signature)
        .expect('Signature: Invalid format.');

    secp256_trait::is_valid_signature(msg_hash.into(), signature.r, signature.s, public_key)
}
