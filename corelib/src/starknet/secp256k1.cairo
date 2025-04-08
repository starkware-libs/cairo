//! Functions and constructs related to elliptic curve operations on the secp256k1 curve.
//!
//! This module provides functionality for performing operations on the secp256k1 elliptic curve,
//! commonly used in cryptographic applications such as Bitcoin and Ethereum.
//! It implements the traits defined in the `secp256_trait` module to ensure consistent behavior
//! across different secp256 curve implementations.
//!
//! Curve information:
//! * Base field: q =
//!   0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f
//! * Scalar field: r =
//!   0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
//! * Curve equation: y^2 = x^3 + 7

use core::gas::GasBuiltin;
use core::option::OptionTrait;
#[allow(unused_imports)]
use starknet::secp256_trait::{
    Secp256PointTrait, Secp256Trait, Signature, is_signature_entry_valid, recover_public_key,
};
#[allow(unused_imports)]
use starknet::{SyscallResult, SyscallResultTrait};

/// A point on the secp256k1 curve.
pub extern type Secp256k1Point;

impl Secp256k1PointCopy of Copy<Secp256k1Point>;
impl Secp256k1PointDrop of Drop<Secp256k1Point>;

pub(crate) impl Secp256k1Impl of Secp256Trait<Secp256k1Point> {
    // TODO(yuval): change to constant once u256 constants are supported.
    fn get_curve_size() -> u256 {
        0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
    }

    fn get_generator_point() -> Secp256k1Point {
        secp256k1_new_syscall(
            0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798,
            0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8,
        )
            .unwrap_syscall()
            .unwrap()
    }

    fn secp256_ec_new_syscall(x: u256, y: u256) -> SyscallResult<Option<Secp256k1Point>> {
        secp256k1_new_syscall(x, y)
    }

    fn secp256_ec_get_point_from_x_syscall(
        x: u256, y_parity: bool,
    ) -> SyscallResult<Option<Secp256k1Point>> {
        secp256k1_get_point_from_x_syscall(x, y_parity)
    }
}

pub(crate) impl Secp256k1PointImpl of Secp256PointTrait<Secp256k1Point> {
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

/// Creates a new point on the secp256k1 curve from its `x` and `y` coordinates.
///
/// # Returns
///
/// Returns `Some(point)` if the coordinates represent a valid point on the curve,
/// `None` otherwise.
extern fn secp256k1_new_syscall(
    x: u256, y: u256,
) -> SyscallResult<Option<Secp256k1Point>> implicits(GasBuiltin, System) nopanic;

/// Adds two points `p0` and `p1` on the secp256k1 curve.
extern fn secp256k1_add_syscall(
    p0: Secp256k1Point, p1: Secp256k1Point,
) -> SyscallResult<Secp256k1Point> implicits(GasBuiltin, System) nopanic;

/// Multiplies a point `p` on the secp256k1 curve by the given `scalar`.
extern fn secp256k1_mul_syscall(
    p: Secp256k1Point, scalar: u256,
) -> SyscallResult<Secp256k1Point> implicits(GasBuiltin, System) nopanic;

/// Recovers a point on the curve given its x-coordinate and y-parity.
///
/// Since the curve equation y² = x³ + 7 has two solutions for y given x,
/// the y_parity parameter is used to determine which y value to use.
///
/// # Arguments
///
/// * `x` - The x coordinate of the point
/// * `y_parity` - If true, choose the odd y value; if false, choose the even y value
///
/// # Returns
///
/// Returns `Some(point)` if a point exists with the given x coordinate,
/// `None` otherwise.
extern fn secp256k1_get_point_from_x_syscall(
    x: u256, y_parity: bool,
) -> SyscallResult<Option<Secp256k1Point>> implicits(GasBuiltin, System) nopanic;

/// Returns the coordinates of a point on the Secp256k1 curve.
extern fn secp256k1_get_xy_syscall(
    p: Secp256k1Point,
) -> SyscallResult<(u256, u256)> implicits(GasBuiltin, System) nopanic;

impl Secp256k1PointSerde of Serde<Secp256k1Point> {
    fn serialize(self: @Secp256k1Point, ref output: Array<felt252>) {
        let point = (*self).get_coordinates().unwrap();
        point.serialize(ref output);
    }

    fn deserialize(ref serialized: Span<felt252>) -> Option<Secp256k1Point> {
        let (x, y) = Serde::<(u256, u256)>::deserialize(ref serialized)?;
        secp256k1_new_syscall(x, y).unwrap_syscall()
    }
}
