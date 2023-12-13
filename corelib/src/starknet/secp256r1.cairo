//! This module contains functions and constructs related to elliptic curve operations on the
//! secp256r1 curve.

use core::option::OptionTrait;
use starknet::{
    EthAddress, secp256_trait::{Secp256Trait, Secp256PointTrait}, SyscallResult, SyscallResultTrait
};

#[derive(Copy, Drop)]
pub extern type Secp256r1Point;

pub(crate) impl Secp256r1Impl of Secp256Trait<Secp256r1Point> {
    // TODO(yuval): change to constant once u256 constants are supported.
    fn get_curve_size() -> u256 {
        0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551
    }
    /// Creates the generator point of the secp256r1 curve.
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
        x: u256, y_parity: bool
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

/// Creates a secp256r1 EC point from the given x and y coordinates.
/// Returns None if the given coordinates do not correspond to a point on the curve.
extern fn secp256r1_new_syscall(
    x: u256, y: u256
) -> SyscallResult<Option<Secp256r1Point>> implicits(GasBuiltin, System) nopanic;

/// Computes the addition of secp256r1 EC points `p0 + p1`.
extern fn secp256r1_add_syscall(
    p0: Secp256r1Point, p1: Secp256r1Point
) -> SyscallResult<Secp256r1Point> implicits(GasBuiltin, System) nopanic;
/// Computes the product of a secp256r1 EC point `p` by the given scalar `scalar`.
extern fn secp256r1_mul_syscall(
    p: Secp256r1Point, scalar: u256
) -> SyscallResult<Secp256r1Point> implicits(GasBuiltin, System) nopanic;

/// Computes the point on the secp256r1 curve that matches the given `x` coordinate, if such exists.
/// Out of the two possible y's, chooses according to `y_parity`.
/// `y_parity` == true means that the y coordinate is odd.
extern fn secp256r1_get_point_from_x_syscall(
    x: u256, y_parity: bool
) -> SyscallResult<Option<Secp256r1Point>> implicits(GasBuiltin, System) nopanic;

/// Returns the coordinates of a point on the secp256r1 curve.
extern fn secp256r1_get_xy_syscall(
    p: Secp256r1Point
) -> SyscallResult<(u256, u256)> implicits(GasBuiltin, System) nopanic;
