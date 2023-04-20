//! This module contains functions and constructs related to elliptic curve operations on the
//! secp256k1 curve.

use integer::{u128_as_non_zero, u128_safe_divmod, U128TryIntoNonZero, U256TryIntoFelt252};
use keccak::keccak_uint256s_be;
use option::OptionTrait;
use result::ResultTrait;
use starknet::{EthAddress, SyscallResult, SyscallResultTrait};
use traits::TryInto;

#[derive(Copy, Drop)]
extern type Secp256K1EcPoint;

/// Creates a secp256k1 EC point from the given x and y coordinates.
/// Returns None if the given coordinates do not correspond to a point on the curve.
extern fn secp256k1_ec_new_syscall(
    x: u256, y: u256
) -> SyscallResult<Option<Secp256K1EcPoint>> implicits(GasBuiltin, System) nopanic;

// TODO(yuval): change to constant once u256 constants are supported.
fn get_N() -> u256 {
    0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
}

/// Computes the addition of secp256k1 EC points `p0 + p1`.
extern fn secp256k1_ec_add_syscall(
    p0: Secp256K1EcPoint, p1: Secp256K1EcPoint
) -> SyscallResult<Secp256K1EcPoint> implicits(GasBuiltin, System) nopanic;
/// Computes the product of a secp256k1 EC point `p` by the given scalar `m`.
extern fn secp256k1_ec_mul_syscall(
    p: Secp256K1EcPoint, m: u256
) -> SyscallResult<Secp256K1EcPoint> implicits(GasBuiltin, System) nopanic;

/// Computes the point on the secp256k1 curve that matches the given `x` coordinate, if such exists.
/// Out of the two possible y's, chooses according to `y_parity`.
extern fn secp256k1_ec_get_point_from_x_syscall(
    x: u256, y_parity: bool
) -> SyscallResult<Option<Secp256K1EcPoint>> implicits(GasBuiltin, System) nopanic;

/// Returns the coordinates of a point on the secp256k1 curve.
extern fn secp256k1_ec_get_coordinates_syscall(
    p: Secp256K1EcPoint
) -> SyscallResult<(u256, u256)> implicits(GasBuiltin, System) nopanic;

/// Creates the generator point of the secp256k1 curve.
fn get_generator_point() -> Secp256K1EcPoint {
    secp256k1_ec_new_syscall(
        u256 { high: 0x79be667ef9dcbbac55a06295ce870b07, low: 0x029bfcdb2dce28d959f2815b16f81798 },
        u256 { high: 0x483ada7726a3c4655da4fbfc0e1108a8, low: 0xfd17b448a68554199c47d08ffb10d4b8 }
    )
        .unwrap_syscall()
        .unwrap()
}

/// Computes a * b^(-1) modulo n.
extern fn u256_div_mod_n(a: u256, b: u256, n: u256) -> u256 implicits(RangeCheck) nopanic;

/// Receives a signature and the signed message hash.
/// Returns the public key associated with the signer, represented as a point on the curve.
/// Note:
///   Some places use non boolean values for v.
///   In that case, `recover_public_key_u32` may be a better match.
fn recover_public_key(
    msg_hash: u256, r: u256, s: u256, y_parity: bool
) -> Option<Secp256K1EcPoint> {
    let r_point = secp256k1_ec_get_point_from_x_syscall(x: r, :y_parity).unwrap_syscall()?;
    let generator_point = get_generator_point();

    // The result is given by
    //   -(msg_hash / r) * gen + (s / r) * r_point
    // where the divisions by `r` are modulo `N`.

    let u1 = u256_div_mod_n(msg_hash, r, get_N());
    let minus_u1 = secp256k1_ec_negate_scalar(u1);
    let u2 = u256_div_mod_n(s, r, get_N());

    let minus_point1 = secp256k1_ec_mul_syscall(generator_point, minus_u1).unwrap_syscall();

    let point2 = secp256k1_ec_mul_syscall(r_point, u2).unwrap_syscall();

    Option::Some(secp256k1_ec_add_syscall(minus_point1, point2).unwrap_syscall())
}

/// Same as recover_public_key, but receives `v` of type `u32` instead of `y_parity`.
/// Uses the parity of `v` as `y_parity`.
fn recover_public_key_u32(msg_hash: u256, r: u256, s: u256, v: u32) -> Option<Secp256K1EcPoint> {
    let y_parity = v % 2 == 0;
    recover_public_key(:msg_hash, :r, :s, :y_parity)
}

/// Computes the negation of a scalar modulo N (the size of the curve).
fn secp256k1_ec_negate_scalar(c: u256) -> u256 {
    get_N() - c
}

/// Verifies a Secp256k1 ECDSA signature.
/// Also verifies that r and s are in the range (0, N).
fn verify_eth_signature(msg_hash: u256, r: u256, s: u256, y_parity: bool, eth_address: EthAddress) {
    assert(is_signature_entry_valid(r), 'Signature out of range');
    assert(is_signature_entry_valid(s), 'Signature out of range');

    let public_key_point = recover_public_key(:msg_hash, :r, :s, :y_parity);
    let calculated_eth_address = public_key_point_to_eth_address(:public_key_point);
    assert(eth_address == calculated_eth_address, 'Invalid signature');
}

/// Checks whether `value` is in the range [1, N).
fn is_signature_entry_valid(value: u256) -> bool {
    value > u256 { high: 0, low: 0 } & value < get_N()
}

/// Converts a public key point to the corresponding Ethereum address.
fn public_key_point_to_eth_address(public_key_point: Secp256K1EcPoint) -> EthAddress {
    let (x, y) = secp256k1_ec_get_coordinates_syscall(public_key_point).unwrap_syscall();
    let keccak_input = ArrayTrait::new();
    keccak_input.push(x);
    keccak_input.push(y);
    let point_hash = keccak_uint256s_be(keccak_input.span());

    // The Ethereum address is the 20 least significant bytes of the keccak of the public key.
    let (_, high_32_bits): (u128, u128) = u128_safe_divmod(
        point_hash.high, 0x100000000_u128.try_into().unwrap()
    );
    let address_u256 = u256 { high: high_32_bits, low: point_hash.low };
    EthAddress { address: address_u256.try_into().unwrap() }
}
