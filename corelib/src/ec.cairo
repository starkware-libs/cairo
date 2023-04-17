use array::ArrayTrait;
use zeroable::IsZeroResult;
use traits::Into;

mod StarkCurve {
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
///
/// * `ec_point_try_new_nz` returns `None` if the point (x, y) is not on the curve.
/// * `ec_point_new_nz` panics in that case.
#[panic_with('not on EC', ec_point_new_nz)]
extern fn ec_point_try_new_nz(x: felt252, y: felt252) -> Option<NonZeroEcPoint> nopanic;

#[inline(always)]
fn ec_point_try_new(x: felt252, y: felt252) -> Option<EcPoint> {
    match ec_point_try_new_nz(:x, :y) {
        Option::Some(pt) => Option::Some(pt.into()),
        Option::None(()) => Option::None(()),
    }
}

fn ec_point_new(x: felt252, y: felt252) -> EcPoint {
    ec_point_new_nz(:x, :y).into()
}

extern fn ec_point_from_x_nz(x: felt252) -> Option<NonZeroEcPoint> implicits(RangeCheck) nopanic;

#[inline(always)]
fn ec_point_from_x(x: felt252) -> Option<EcPoint> {
    match ec_point_from_x_nz(:x) {
        Option::Some(pt) => Option::Some(pt.into()),
        Option::None(()) => Option::None(()),
    }
}

extern fn ec_point_unwrap(p: NonZeroEcPoint) -> (felt252, felt252) nopanic;
/// Computes the negation of an elliptic curve point (-p).
extern fn ec_neg(p: EcPoint) -> EcPoint nopanic;
/// Checks whether the given `EcPoint` is the zero point.
extern fn ec_point_is_zero(p: EcPoint) -> IsZeroResult<EcPoint> nopanic;

/// Converts `p` to `NonZeroEcPoint`. Panics if `p` is the zero point.
fn ec_point_non_zero(p: EcPoint) -> NonZeroEcPoint {
    match ec_point_is_zero(p) {
        IsZeroResult::Zero(()) => panic_with_felt252('Zero point'),
        IsZeroResult::NonZero(p_nz) => p_nz,
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
/// Finalizes the EC computation and returns the result (returns `None` if the result is the
/// zero point).
extern fn ec_state_try_finalize_nz(s: EcState) -> Option<NonZeroEcPoint> nopanic;
/// Adds the product p * m to the state.
extern fn ec_state_add_mul(ref s: EcState, m: felt252, p: NonZeroEcPoint) implicits(EcOp) nopanic;

/// Finalizes the EC computation and returns the result.
#[inline(always)]
fn ec_state_finalize(s: EcState) -> EcPoint {
    match ec_state_try_finalize_nz(s) {
        Option::Some(pt) => pt.into(),
        Option::None(()) => ec_point_zero(),
    }
}

/// Computes the product of an EC point `p` by the given scalar `m`.
fn ec_mul(p: EcPoint, m: felt252) -> EcPoint {
    match ec_point_is_zero(p) {
        IsZeroResult::Zero(()) => p,
        IsZeroResult::NonZero(p_nz) => {
            let mut state = ec_state_init();
            ec_state_add_mul(ref state, m, p_nz);
            ec_state_finalize(state)
        }
    }
}

impl EcPointAdd of Add<EcPoint> {
    /// Computes the sum of two points on the curve.
    // TODO(lior): Implement using a libfunc to make it more efficient.
    fn add(lhs: EcPoint, rhs: EcPoint) -> EcPoint {
        let lhs_nz = match ec_point_is_zero(lhs) {
            IsZeroResult::Zero(()) => {
                return rhs;
            },
            IsZeroResult::NonZero(pt) => pt,
        };
        let rhs_nz = match ec_point_is_zero(rhs) {
            IsZeroResult::Zero(()) => {
                return lhs;
            },
            IsZeroResult::NonZero(pt) => pt,
        };
        let mut state = ec_state_init();
        ec_state_add(ref state, lhs_nz);
        ec_state_add(ref state, rhs_nz);
        ec_state_finalize(state)
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
        match ec_point_is_zero(rhs) {
            IsZeroResult::Zero(()) => {
                // lhs - 0 = lhs.
                return lhs;
            },
            IsZeroResult::NonZero(_) => {},
        };
        // lhs - rhs = lhs + (-rhs).
        lhs + ec_neg(rhs)
    }
}

impl EcPointSubEq of SubEq<EcPoint> {
    #[inline(always)]
    fn sub_eq(ref self: EcPoint, other: EcPoint) {
        self = Sub::sub(self, other);
    }
}
