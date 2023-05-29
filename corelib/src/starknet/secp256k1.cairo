//! This module contains functions and constructs related to elliptic curve operations on the
//! secp256k1 curve.

use super::secp256::{Secp256Trait};

use option::OptionTrait;
use starknet::{EthAddress, SyscallResult, SyscallResultTrait};

#[derive(Copy, Drop)]
extern type Secp256k1EcPoint;

impl Secp256K1Impl of Secp256Trait {
    // TODO(yuval): change to constant once u256 constants are supported.
    fn get_curve_size() -> u256 {
        0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
    }
    /// Creates the generator point of the secp256k1 curve.
    fn get_generator_point() -> Secp256EcPoint {
        secp256k1_ec_new_syscall(
            0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798,
            0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8
        )
            .unwrap_syscall()
            .unwrap()
    }

    fn secp256_ec_new_syscall(x: u256, y: u256) -> SyscallResult<Option<Secp256EcPoint>> {
        secp256k1_ec_new_syscall(x, y)
    }
    fn secp256_ec_add_syscall(
        p0: Secp256EcPoint, p1: Secp256EcPoint
    ) -> SyscallResult<Secp256EcPoint> {
        secp256k1_ec_add_syscall(p0, p1)
    }
    fn secp256_ec_mul_syscall(p: Secp256EcPoint, m: u256) -> SyscallResult<Secp256EcPoint> {
        secp256k1_ec_mul_syscall(p, m)
    }
    fn secp256_ec_get_point_from_x_syscall(
        x: u256, y_parity: bool
    ) -> SyscallResult<Option<Secp256EcPoint>> {
        secp256k1_ec_get_point_from_x_syscall(x, y_parity)
    }
    fn secp256_ec_get_coordinates_syscall(p: Secp256EcPoint) -> SyscallResult<(u256, u256)> {
        secp256k1_ec_get_coordinates_syscall(p)
    }

    fn recover_public_key(
        msg_hash: u256, r: u256, s: u256, y_parity: bool
    ) -> Option<Secp256EcPoint> {
        super::secp256::recover_public_key(msg_hash, r, s, y_parity)
    }
    fn recover_public_key_u32(msg_hash: u256, r: u256, s: u256, v: u32) -> Option<Secp256EcPoint> {
        super::secp256::recover_public_key_u32(msg_hash, r, s, v)
    }

    fn verify_eth_signature(
        msg_hash: u256, r: u256, s: u256, y_parity: bool, eth_address: EthAddress
    ) {
        super::secp256::verify_eth_signature(msg_hash, r, s, y_parity, eth_address)
    }
    fn verify_eth_signature_u32(msg_hash: u256, r: u256, s: u256, v: u32, eth_address: EthAddress) {
        super::secp256::verify_eth_signature_u32(msg_hash, r, s, v, eth_address)
    }
}

/// Creates a secp256k1 EC point from the given x and y coordinates.
/// Returns None if the given coordinates do not correspond to a point on the curve.
extern fn secp256k1_ec_new_syscall(
    x: u256, y: u256
) -> SyscallResult<Option<Secp256EcPoint>> implicits(GasBuiltin, System) nopanic;

/// Computes the addition of secp256k1 EC points `p0 + p1`.
extern fn secp256k1_ec_add_syscall(
    p0: Secp256EcPoint, p1: Secp256EcPoint
) -> SyscallResult<Secp256EcPoint> implicits(GasBuiltin, System) nopanic;
/// Computes the product of a secp256k1 EC point `p` by the given scalar `m`.
extern fn secp256k1_ec_mul_syscall(
    p: Secp256EcPoint, m: u256
) -> SyscallResult<Secp256EcPoint> implicits(GasBuiltin, System) nopanic;

/// Computes the point on the secp256k1 curve that matches the given `x` coordinate, if such exists.
/// Out of the two possible y's, chooses according to `y_parity`.
extern fn secp256k1_ec_get_point_from_x_syscall(
    x: u256, y_parity: bool
) -> SyscallResult<Option<Secp256EcPoint>> implicits(GasBuiltin, System) nopanic;

/// Returns the coordinates of a point on the secp256k1 curve.
extern fn secp256k1_ec_get_coordinates_syscall(
    p: Secp256EcPoint
) -> SyscallResult<(u256, u256)> implicits(GasBuiltin, System) nopanic;
