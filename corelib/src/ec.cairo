//! This module contains functions and constructs related to elliptic curve operations on the Stark
//! curve.

use array::ArrayTrait;
use traits::{Into, TryInto};
use zeroable::IsZeroResult;

mod stark_curve {
    /// The STARK Curve is defined by the equation `y^2 = x^3 + ALPHA*x + BETA`.
    const ALPHA: felt252 = 1;
    /// The STARK Curve is defined by the equation `y^2 = x^3 + ALPHA*x + BETA`.
    const BETA: felt252 = 0x6f21413efbe40de150e596d72f7a8c5609ad26c15c915c1f4cdfcb99cee9e89;
    /// The order (number of points) of the STARK Curve.
    const ORDER: felt252 = 0x800000000000010ffffffffffffffffb781126dcae7b2321e66a241adc64d2f;
    /// The x coordinate of the generator point used in the ECDSA signature.
    const GEN_X: felt252 = 0x1ef15c18599971b7beced415a40f0c7deacfd9b0d1819e03d723d8bc943cfca;
    /// The y coordinate of the generator point used in the ECDSA signature.
    const GEN_Y: felt252 = 0x5668060aa49730b7be4801df46ec62de53ecd11abe43a32873000c36e8dc1f;
}

extern type EcOp;
#[derive(Copy, Drop)]
extern type EcPoint;
type NonZeroEcPoint = NonZero<EcPoint>;

/// Returns the zero point of the curve ("the point at infinity").
extern fn ec_point_zero() -> EcPoint nopanic;
/// Constructs a non-zero point from its (x, y) coordinates.
/// Returns `None` if the point (x, y) is not on the curve.
extern fn ec_point_try_new_nz(x: felt252, y: felt252) -> Option<NonZeroEcPoint> nopanic;
/// Constructs a non-zero point from its x coordinate.
/// Returns `None` if no point of form (x, _) is on the curve.
extern fn ec_point_from_x_nz(x: felt252) -> Option<NonZeroEcPoint> implicits(RangeCheck) nopanic;
/// Unwraps a non-zero point into its (x, y) coordinates.
extern fn ec_point_unwrap(p: NonZeroEcPoint) -> (felt252, felt252) nopanic;
/// Computes the negation of an elliptic curve point (-p).
extern fn ec_neg(p: EcPoint) -> EcPoint nopanic;
/// Checks whether the given `EcPoint` is the zero point.
extern fn ec_point_is_zero(p: EcPoint) -> IsZeroResult<EcPoint> nopanic;

/// Converts `EcPoint` to `NonZeroEcPoint`.
impl EcPointTryIntoNonZero of TryInto<EcPoint, NonZeroEcPoint> {
    #[inline(always)]
    fn try_into(self: EcPoint) -> Option<NonZeroEcPoint> {
        match ec_point_is_zero(self) {
            IsZeroResult::Zero => Option::None,
            IsZeroResult::NonZero(p_nz) => Option::Some(p_nz),
        }
    }
}

// EC state.

// TODO(lior): Allow explicit clone() for EcState, since we don't allow implicit dup (Copy).
#[derive(Drop)]
extern type EcState;

/// Initializes an EC computation with the zero point.
extern fn ec_state_init() -> EcState nopanic;

/// Adds a point to the computation.
extern fn ec_state_add(ref s: EcState, p: NonZeroEcPoint) nopanic;
/// Adds the product p * scalar to the state.
extern fn ec_state_add_mul(
    ref s: EcState, scalar: felt252, p: NonZeroEcPoint
) implicits(EcOp) nopanic;
/// Finalizes the EC computation and returns the result (returns `None` if the result is the
/// zero point).
extern fn ec_state_try_finalize_nz(s: EcState) -> Option<NonZeroEcPoint> nopanic;

#[generate_trait]
impl EcStateImpl of EcStateTrait {
    /// Initializes an EC computation with the zero point.
    fn init() -> EcState {
        ec_state_init()
    }
    /// Adds a point to the computation.
    #[inline(always)]
    fn add(ref self: EcState, p: NonZeroEcPoint) {
        ec_state_add(ref self, :p);
    }
    /// Adds the product p * scalar to the state.
    #[inline(always)]
    fn add_mul(ref self: EcState, scalar: felt252, p: NonZeroEcPoint) {
        ec_state_add_mul(ref self, :scalar, :p);
    }
    /// Finalizes the EC computation and returns the result (returns `None` if the result is the
    /// zero point).
    #[inline(always)]
    fn finalize_nz(self: EcState) -> Option<NonZeroEcPoint> {
        ec_state_try_finalize_nz(self)
    }
    /// Finalizes the EC computation and returns the result.
    #[inline(always)]
    fn finalize(self: EcState) -> EcPoint {
        match self.finalize_nz() {
            Option::Some(p_nz) => p_nz.into(),
            Option::None => ec_point_zero(),
        }
    }
}

#[generate_trait]
impl EcPointImpl of EcPointTrait {
    /// Creates a new EC point from its (x, y) coordinates.
    #[inline(always)]
    fn new(x: felt252, y: felt252) -> Option<EcPoint> {
        Option::Some(ec_point_try_new_nz(:x, :y)?.into())
    }
    /// Creates a new EC point from its x coordinate.
    #[inline(always)]
    fn new_from_x(x: felt252) -> Option<EcPoint> {
        Option::Some(ec_point_from_x_nz(:x)?.into())
    }
    /// Returns the coordinates of the EC point.
    #[inline(always)]
    fn coordinates(self: NonZeroEcPoint) -> (felt252, felt252) {
        ec_point_unwrap(self)
    }
    /// Computes the product of an EC point `p` by the given scalar `scalar`.
    fn mul(self: EcPoint, scalar: felt252) -> EcPoint {
        match self.try_into() {
            Option::Some(self_nz) => {
                let mut state = EcStateTrait::init();
                state.add_mul(scalar, self_nz);
                state.finalize()
            },
            Option::None => self,
        }
    }
}

impl EcPointNeg of Neg<EcPoint> {
    fn neg(a: EcPoint) -> EcPoint {
        ec_neg(a)
    }
}

impl EcPointAdd of Add<EcPoint> {
    /// Computes the sum of two points on the curve.
    // TODO(lior): Implement using a libfunc to make it more efficient.
    fn add(lhs: EcPoint, rhs: EcPoint) -> EcPoint {
        let lhs_nz = match lhs.try_into() {
            Option::Some(pt) => pt,
            Option::None => { return rhs; },
        };
        let rhs_nz = match rhs.try_into() {
            Option::Some(pt) => pt,
            Option::None => { return lhs; },
        };
        let mut state = ec_state_init();
        state.add(lhs_nz);
        state.add(rhs_nz);
        state.finalize()
    }
}

impl EcPointAddEq of AddEq<EcPoint> {
    #[inline(always)]
    fn add_eq(ref self: EcPoint, other: EcPoint) {
        self = Add::add(self, other);
    }
}

impl EcPointSub of Sub<EcPoint> {
    /// Computes the difference between two points on the curve.
    fn sub(lhs: EcPoint, rhs: EcPoint) -> EcPoint {
        let nz_point: Option<NonZero<EcPoint>> = rhs.try_into();
        match nz_point {
            Option::Some(_) => {},
            Option::None => {
                // lhs - 0 = lhs.
                return lhs;
            },
        };
        // lhs - rhs = lhs + (-rhs).
        lhs + (-rhs)
    }
}

impl EcPointSubEq of SubEq<EcPoint> {
    #[inline(always)]
    fn sub_eq(ref self: EcPoint, other: EcPoint) {
        self = Sub::sub(self, other);
    }
}
