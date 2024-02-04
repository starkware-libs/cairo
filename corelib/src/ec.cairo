//! This module contains functions and constructs related to elliptic curve operations on the Stark
//! curve.

use core::array::ArrayTrait;
use core::traits::{Into, TryInto};
use core::zeroable::IsZeroResult;

pub mod stark_curve {
    /// The STARK Curve is defined by the equation `y^2 = x^3 + ALPHA*x + BETA`.
    pub const ALPHA: felt252 = 1;
    /// The STARK Curve is defined by the equation `y^2 = x^3 + ALPHA*x + BETA`.
    pub const BETA: felt252 = 0x6f21413efbe40de150e596d72f7a8c5609ad26c15c915c1f4cdfcb99cee9e89;
    /// The order (number of points) of the STARK Curve.
    pub const ORDER: felt252 = 0x800000000000010ffffffffffffffffb781126dcae7b2321e66a241adc64d2f;
    /// The x coordinate of the generator point used in the ECDSA signature.
    pub const GEN_X: felt252 = 0x1ef15c18599971b7beced415a40f0c7deacfd9b0d1819e03d723d8bc943cfca;
    /// The y coordinate of the generator point used in the ECDSA signature.
    pub const GEN_Y: felt252 = 0x5668060aa49730b7be4801df46ec62de53ecd11abe43a32873000c36e8dc1f;
}

pub extern type EcOp;
#[derive(Copy, Drop)]
pub extern type EcPoint;
pub type NonZeroEcPoint = NonZero<EcPoint>;

/// Returns the zero point of the curve ("the point at infinity").
extern fn ec_point_zero() -> EcPoint nopanic;
/// Constructs a non-zero point from its (x, y) coordinates.
/// Returns `None` if the point (x, y) is not on the curve.
extern fn ec_point_try_new_nz(x: felt252, y: felt252) -> Option<NonZeroEcPoint> nopanic;
/// Constructs a non-zero point from its x coordinate.
/// Returns `None` if no point of form (x, _) is on the curve.
extern fn ec_point_from_x_nz(x: felt252) -> Option<NonZeroEcPoint> implicits(RangeCheck) nopanic;
/// Unwraps a non-zero point into its (x, y) coordinates.
pub extern fn ec_point_unwrap(p: NonZeroEcPoint) -> (felt252, felt252) nopanic;
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
#[derive(Drop)]
pub extern type EcState;

mod internal {
    impl EcStateCopy of Copy<super::EcState>;
    pub impl EcStateClone of Clone<super::EcState> {
        #[inline(always)]
        fn clone(self: @super::EcState) -> super::EcState {
            *self
        }
    }
}
impl EcStateClone = internal::EcStateClone;

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
pub impl EcStateImpl of EcStateTrait {
    /// Initializes an EC computation with the zero point.
    #[must_use]
    fn init() -> EcState nopanic {
        ec_state_init()
    }
    /// Adds a point to the computation.
    #[inline(always)]
    fn add(ref self: EcState, p: NonZeroEcPoint) nopanic {
        ec_state_add(ref self, :p);
    }
    /// Subs a point to the computation.
    #[inline(always)]
    fn sub(ref self: EcState, p: NonZeroEcPoint) {
        // TODO(orizi): Have a `ec_neg` for NonZeroEcPoint as well, or a `ec_state_sub`.
        let p: EcPoint = p.into();
        let p_neg = ec_neg(p);
        let p_neg_nz = p_neg.try_into().unwrap();
        ec_state_add(ref self, p_neg_nz);
    }
    /// Adds the product p * scalar to the state.
    #[inline(always)]
    fn add_mul(ref self: EcState, scalar: felt252, p: NonZeroEcPoint) nopanic {
        ec_state_add_mul(ref self, :scalar, :p);
    }
    /// Finalizes the EC computation and returns the result (returns `None` if the result is the
    /// zero point).
    #[inline(always)]
    fn finalize_nz(self: EcState) -> Option<NonZeroEcPoint> nopanic {
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
pub impl EcPointImpl of EcPointTrait {
    /// Creates a new EC point from its (x, y) coordinates.
    #[inline(always)]
    fn new(x: felt252, y: felt252) -> Option<EcPoint> {
        Option::Some(EcPointTrait::new_nz(:x, :y)?.into())
    }
    /// Creates a new NonZero EC point from its (x, y) coordinates.
    #[inline(always)]
    fn new_nz(x: felt252, y: felt252) -> Option<NonZeroEcPoint> {
        ec_point_try_new_nz(:x, :y)
    }
    /// Creates a new EC point from its x coordinate.
    #[inline(always)]
    fn new_from_x(x: felt252) -> Option<EcPoint> {
        Option::Some(EcPointTrait::new_nz_from_x(:x)?.into())
    }
    /// Creates a new NonZero EC point from its x coordinate.
    #[inline(always)]
    fn new_nz_from_x(x: felt252) -> Option<NonZeroEcPoint> {
        ec_point_from_x_nz(:x)
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
