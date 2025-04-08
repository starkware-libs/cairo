//! Functions and constructs related to elliptic curve operations on the secp256r1 curve.
//!
//! This module provides functionality for performing operations on the NIST P-256 (also known as
//! secp256r1) elliptic curve. It implements the traits defined in the `secp256_trait` module to
//! ensure consistent behavior across different secp256 curve implementations.
//!
//! Curve information:
//! * Base field: q =
//!   0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff
//! * Scalar field: r =
//!   0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551
//! * a = -3
//! * b = 0x5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b
//! * Curve equation: y^2 = x^3 + ax + b

use core::gas::GasBuiltin;
use core::option::OptionTrait;
#[allow(unused_imports)]
use starknet::secp256_trait::{Secp256PointTrait, Secp256Trait};
#[allow(unused_imports)]
use starknet::{EthAddress, SyscallResult, SyscallResultTrait};

/// Represents a point on the secp256r1 elliptic curve.
pub extern type Secp256r1Point;

impl Secp256r1PointCopy of Copy<Secp256r1Point>;
impl Secp256r1PointDrop of Drop<Secp256r1Point>;

pub(crate) impl Secp256r1Impl of Secp256Trait<Secp256r1Point> {
    // TODO(yuval): change to constant once u256 constants are supported.
    fn get_curve_size() -> u256 {
        0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551
    }

    fn get_generator_point() -> Secp256r1Point {
        secp256r1_new_syscall(
            0x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296,
            0x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5,
        )
            .unwrap_syscall()
            .unwrap()
    }

    fn secp256_ec_new_syscall(x: u256, y: u256) -> SyscallResult<Option<Secp256r1Point>> {
        secp256r1_new_syscall(x, y)
    }

    fn secp256_ec_get_point_from_x_syscall(
        x: u256, y_parity: bool,
    ) -> SyscallResult<Option<Secp256r1Point>> {
        secp256r1_get_point_from_x_syscall(x, y_parity)
    }
}

pub(crate) impl Secp256r1PointImpl of Secp256PointTrait<Secp256r1Point> {
    fn get_coordinates(self: Secp256r1Point) -> SyscallResult<(u256, u256)> {
        secp256r1_get_xy_syscall(self)
    }

    fn add(self: Secp256r1Point, other: Secp256r1Point) -> SyscallResult<Secp256r1Point> {
        secp256r1_add_syscall(self, other)
    }

    fn mul(self: Secp256r1Point, scalar: u256) -> SyscallResult<Secp256r1Point> {
        secp256r1_mul_syscall(self, scalar)
    }
}

/// Creates a new point on the secp256r1 curve from its `x` and `y` coordinates.
///
/// # Returns
///
/// Returns `Some(point)` if the coordinates represent a valid point on the curve,
/// `None` otherwise.
extern fn secp256r1_new_syscall(
    x: u256, y: u256,
) -> SyscallResult<Option<Secp256r1Point>> implicits(GasBuiltin, System) nopanic;

/// Adds two points `p0` and `p1` on the secp256r1 curve.
extern fn secp256r1_add_syscall(
    p0: Secp256r1Point, p1: Secp256r1Point,
) -> SyscallResult<Secp256r1Point> implicits(GasBuiltin, System) nopanic;

/// Multiplies a point `p` on the curve by the given `scalar`.
extern fn secp256r1_mul_syscall(
    p: Secp256r1Point, scalar: u256,
) -> SyscallResult<Secp256r1Point> implicits(GasBuiltin, System) nopanic;

/// Recovers a point on the curve given its x-coordinate and `y-parity`.
///
/// Since the curve equation y² = x³ + ax + b has two solutions for y given x,
/// the `y_parity` parameter is used to determine which y value to use.
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
extern fn secp256r1_get_point_from_x_syscall(
    x: u256, y_parity: bool,
) -> SyscallResult<Option<Secp256r1Point>> implicits(GasBuiltin, System) nopanic;

/// Returns the coordinates of a point on the secp256r1 curve.
extern fn secp256r1_get_xy_syscall(
    p: Secp256r1Point,
) -> SyscallResult<(u256, u256)> implicits(GasBuiltin, System) nopanic;

impl Secp256r1PointSerde of Serde<Secp256r1Point> {
    fn serialize(self: @Secp256r1Point, ref output: Array<felt252>) {
        let point = (*self).get_coordinates().unwrap();
        point.serialize(ref output)
    }

    fn deserialize(ref serialized: Span<felt252>) -> Option<Secp256r1Point> {
        let (x, y) = Serde::<(u256, u256)>::deserialize(ref serialized)?;
        secp256r1_new_syscall(x, y).unwrap_syscall()
    }
}
