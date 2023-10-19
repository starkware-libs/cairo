//! This module contains functions and constructs related to elliptic curve operations on the
//! secp256k1 curve.

use option::OptionTrait;
use starknet::{
    EthAddress,
    secp256_trait::{
        Secp256Trait, Secp256PointTrait, recover_public_key, is_signature_entry_valid, Signature
    },
    SyscallResult, SyscallResultTrait
};
use keccak::keccak_u256s_be_inputs;


#[derive(Copy, Drop)]
extern type Secp256k1Point;

impl Secp256k1Impl of Secp256Trait<Secp256k1Point> {
    // TODO(yuval): change to constant once u256 constants are supported.
    fn get_curve_size() -> u256 {
        0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
    }
    /// Creates the generator point of the secp256k1 curve.
    fn get_generator_point() -> Secp256k1Point {
        secp256k1_new_syscall(
            0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798,
            0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8
        )
            .unwrap_syscall()
            .unwrap()
    }

    fn secp256_ec_new_syscall(x: u256, y: u256) -> SyscallResult<Option<Secp256k1Point>> {
        secp256k1_new_syscall(x, y)
    }
    fn secp256_ec_get_point_from_x_syscall(
        x: u256, y_parity: bool
    ) -> SyscallResult<Option<Secp256k1Point>> {
        secp256k1_get_point_from_x_syscall(x, y_parity)
    }
}

impl Secp256k1PointImpl of Secp256PointTrait<Secp256k1Point> {
    fn get_coordinates(self: Secp256k1Point) -> SyscallResult<(u256, u256)> {
        secp256k1_get_xy_syscall(self)
    }
    fn add(self: Secp256k1Point, other: Secp256k1Point) -> SyscallResult<Secp256k1Point> {
        secp256k1_add_syscall(self, other)
    }
    fn mul(self: Secp256k1Point, scalar: u256) -> SyscallResult<Secp256k1Point> {
        secp256k1_mul_syscall(self, scalar)
    }
}

/// Creates a secp256k1 EC point from the given x and y coordinates.
/// Returns None if the given coordinates do not correspond to a point on the curve.
extern fn secp256k1_new_syscall(
    x: u256, y: u256
) -> SyscallResult<Option<Secp256k1Point>> implicits(GasBuiltin, System) nopanic;

/// Computes the addition of secp256k1 EC points `p0 + p1`.
extern fn secp256k1_add_syscall(
    p0: Secp256k1Point, p1: Secp256k1Point
) -> SyscallResult<Secp256k1Point> implicits(GasBuiltin, System) nopanic;
/// Computes the product of a secp256k1 EC point `p` by the given scalar `scalar`.
extern fn secp256k1_mul_syscall(
    p: Secp256k1Point, scalar: u256
) -> SyscallResult<Secp256k1Point> implicits(GasBuiltin, System) nopanic;

/// Computes the point on the secp256k1 curve that matches the given `x` coordinate, if such exists.
/// Out of the two possible y's, chooses according to `y_parity`.
/// `y_parity` == true means that the y coordinate is odd.
extern fn secp256k1_get_point_from_x_syscall(
    x: u256, y_parity: bool
) -> SyscallResult<Option<Secp256k1Point>> implicits(GasBuiltin, System) nopanic;

/// Returns the coordinates of a point on the secp256k1 curve.
extern fn secp256k1_get_xy_syscall(
    p: Secp256k1Point
) -> SyscallResult<(u256, u256)> implicits(GasBuiltin, System) nopanic;

/// Asserts that a Secp256 ECDSA signature is valid.
/// Also verifies that r and s components of the signature are in the range (0, N),
/// where N is the size of the curve.
fn verify_eth_signature(msg_hash: u256, signature: Signature, eth_address: EthAddress) {
    match is_eth_signature_valid(:msg_hash, :signature, :eth_address) {
        Result::Ok(()) => {},
        Result::Err(err) => panic_with_felt252(err),
    }
}

/// Checks a Secp256 ECDSA signature.
/// Also verifies that r and s components of the signature are in the range (0, N),
/// where N is the size of the curve.
/// Returns a Result with an error string if the signature is invalid.
fn is_eth_signature_valid(
    msg_hash: u256, signature: Signature, eth_address: EthAddress
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
fn public_key_point_to_eth_address<
    Secp256Point, +Drop<Secp256Point>, +Secp256Trait<Secp256Point>, +Secp256PointTrait<Secp256Point>
>(
    public_key_point: Secp256Point
) -> EthAddress {
    let (x, y) = public_key_point.get_coordinates().unwrap_syscall();

    // Keccak output is little endian.
    let point_hash_le = keccak_u256s_be_inputs(array![x, y].span());
    let point_hash = u256 {
        low: integer::u128_byte_reverse(point_hash_le.high),
        high: integer::u128_byte_reverse(point_hash_le.low)
    };

    point_hash.into()
}
