// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (account/src/utils/secp256_point.cairo)

use core::fmt::{Error, Formatter};
use starknet::SyscallResultTrait;
use starknet::secp256_trait::{Secp256PointTrait, Secp256Trait};
use starknet::storage_access::StorePacking;

/// Packs a Secp256Point into a (felt252, felt252).
///
/// The packing is done as follows:
///
/// - First felt contains x.low (x being the x-coordinate of the point).
/// - Second felt contains x.high and the parity bit, at the least significant bits (2 * x.high +
/// parity).
pub impl Secp256PointStorePacking<
    Secp256Point, +Secp256Trait<Secp256Point>, +Secp256PointTrait<Secp256Point>,
> of StorePacking<Secp256Point, (felt252, felt252)> {
    fn pack(value: Secp256Point) -> (felt252, felt252) {
        let (x, y) = value.get_coordinates().unwrap_syscall();

        let parity = y.low % 2;
        let xhigh_and_parity = 2 * x.high.into() + parity.into();

        (x.low.into(), xhigh_and_parity)
    }

    fn unpack(value: (felt252, felt252)) -> Secp256Point {
        let (xlow, xhigh_and_parity) = value;
        let xhigh_and_parity: u256 = xhigh_and_parity.into();

        let low = xlow.try_into().unwrap();
        let high = (xhigh_and_parity / 2).try_into().unwrap();

        let x = u256 { low, high };
        let parity = xhigh_and_parity % 2 == 1;

        // Expects parity odd to be true
        Secp256Trait::secp256_ec_get_point_from_x_syscall(x, parity)
            .unwrap_syscall()
            .expect('Secp256Point: Invalid point.')
    }
}

pub impl Secp256PointPartialEq<
    Secp256Point, +Secp256PointTrait<Secp256Point>, +Copy<Secp256Point>,
> of PartialEq<Secp256Point> {
    #[inline(always)]
    fn eq(lhs: @Secp256Point, rhs: @Secp256Point) -> bool {
        (*lhs).get_coordinates().unwrap_syscall() == (*rhs).get_coordinates().unwrap_syscall()
    }
    #[inline(always)]
    fn ne(lhs: @Secp256Point, rhs: @Secp256Point) -> bool {
        !(lhs == rhs)
    }
}

pub impl DebugSecp256Point<
    Secp256Point, +Secp256PointTrait<Secp256Point>, +Copy<Secp256Point>,
> of core::fmt::Debug<Secp256Point> {
    fn fmt(self: @Secp256Point, ref f: Formatter) -> Result<(), Error> {
        let (x, y) = (*self).get_coordinates().unwrap_syscall();
        write!(f, "({x:?},{y:?})")
    }
}
