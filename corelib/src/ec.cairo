//! Functions and constructs related to elliptic curve operations on the STARK curve.
//!
//! This module provides implementations for various elliptic curve operations tailored for the
//! STARK curve.
//!
//! Curve information:
//! * Curve equation: y² ≡ x³ + α·x + β (mod p)
//! * α = 1
//! * β = 0x6f21413efbe40de150e596d72f7a8c5609ad26c15c915c1f4cdfcb99cee9e89
//! * p = 0x0800000000000011000000000000000000000000000000000000000000000001 = 2^251 + 17 * 2^192 +
//! 1
//! Generator point:
//! * x = 0x1ef15c18599971b7beced415a40f0c7deacfd9b0d1819e03d723d8bc943cfca
//! * y = 0x5668060aa49730b7be4801df46ec62de53ecd11abe43a32873000c36e8dc1f
//!
//! # Examples
//!
//! Creating points and basic operations:
//!
//! ```
//! // Create a point from coordinates
//! let point = EcPointTrait::new(
//!     x: 336742005567258698661916498343089167447076063081786685068305785816009957563,
//!     y: 1706004133033694959518200210163451614294041810778629639790706933324248611779,
//! ).unwrap();
//!
//! // Perform scalar multiplication
//! let result = point.mul(2);
//!
//! // Add points
//! let sum = point + result;
//!
//! // Subtract points
//! let diff = result - point;
//! ```
//!
//! Using EC state for batch operations:
//!
//! ```
//! let p = EcPointTrait::new_from_x(1).unwrap();
//! let p_nz = p.try_into().unwrap();
//!
//! // Initialize state
//! let mut state = EcStateTrait::init();
//!
//! // Add points and scalar multiplications
//! state.add(p_nz);
//! state.add_mul(1, p_nz);
//!
//! // Get the final result
//! let _result = state.finalize();
//! ```

use crate::RangeCheck;
#[allow(unused_imports)]
use crate::array::ArrayTrait;
#[allow(unused_imports)]
use crate::traits::{Into, TryInto};
use crate::zeroable::IsZeroResult;

pub mod stark_curve {
    /// The STARK Curve is defined by the equation y² ≡ x³ + α·x + β (mod p).
    pub const ALPHA: felt252 = 1;
    /// The STARK Curve is defined by the equation y² ≡ x³ + α·x + β (mod p).
    pub const BETA: felt252 = 0x6f21413efbe40de150e596d72f7a8c5609ad26c15c915c1f4cdfcb99cee9e89;
    /// The order (number of points) of the STARK Curve.
    pub const ORDER: felt252 = 0x800000000000010ffffffffffffffffb781126dcae7b2321e66a241adc64d2f;
    /// The x coordinate of the generator point used in the ECDSA signature.
    pub const GEN_X: felt252 = 0x1ef15c18599971b7beced415a40f0c7deacfd9b0d1819e03d723d8bc943cfca;
    /// The y coordinate of the generator point used in the ECDSA signature.
    pub const GEN_Y: felt252 = 0x5668060aa49730b7be4801df46ec62de53ecd11abe43a32873000c36e8dc1f;
}

pub extern type EcOp;

/// A point on the STARK curve.
///
/// Points can be created using [`EcPointTrait::new`] or [`EcPointTrait::new_from_x`].
/// The zero point represents the point at infinity.
pub extern type EcPoint;

impl EcPointCopy of Copy<EcPoint>;
impl EcPointDrop of Drop<EcPoint>;

/// A non-zero point on the STARK curve (cannot be the point at infinity).
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
    #[inline]
    fn try_into(self: EcPoint) -> Option<NonZeroEcPoint> {
        match ec_point_is_zero(self) {
            IsZeroResult::Zero => None,
            IsZeroResult::NonZero(p_nz) => Some(p_nz),
        }
    }
}

/// Elliptic curve state.
///
/// Use this to perform multiple point operations efficiently.
/// Initialize with [`EcStateTrait::init`], add points with [`EcStateTrait::add`]
/// or [`EcStateTrait::add_mul`], and finalize with [`EcStateTrait::finalize`].
pub extern type EcState;

impl EcStateDrop of Drop<EcState>;

mod internal {
    impl EcStateCopy of Copy<super::EcState>;
    pub impl EcStateClone of Clone<super::EcState> {
        #[inline]
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

/// Adds the product `p * scalar` to the state.
extern fn ec_state_add_mul(
    ref s: EcState, scalar: felt252, p: NonZeroEcPoint,
) implicits(EcOp) nopanic;

/// Finalizes the EC computation and returns the result (returns `None` if the result is the
/// zero point).
extern fn ec_state_try_finalize_nz(s: EcState) -> Option<NonZeroEcPoint> nopanic;

#[generate_trait]
pub impl EcStateImpl of EcStateTrait {
    /// Initializes an EC computation with the zero point.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut state = EcStateTrait::init();
    /// ```
    #[must_use]
    fn init() -> EcState nopanic {
        ec_state_init()
    }

    /// Adds a point to the computation.
    ///
    /// # Arguments
    ///
    /// * `p` - The non-zero point to add
    #[inline]
    fn add(ref self: EcState, p: NonZeroEcPoint) nopanic {
        ec_state_add(ref self, :p);
    }

    /// Subtracts a point to the computation.
    ///
    /// # Arguments
    ///
    /// * `p` - The non-zero point to subtract
    #[inline]
    fn sub(ref self: EcState, p: NonZeroEcPoint) {
        // TODO(orizi): Have a `ec_neg` for NonZeroEcPoint as well, or a `ec_state_sub`.
        let p: EcPoint = p.into();
        let p_neg = ec_neg(p);
        let p_neg_nz = p_neg.try_into().unwrap();
        ec_state_add(ref self, p_neg_nz);
    }

    /// Adds the product `p * scalar` to the state.
    ///
    /// # Arguments
    ///
    /// * `scalar` - The scalar to multiply the point by
    /// * `p` - The non-zero point to multiply and add
    #[inline]
    fn add_mul(ref self: EcState, scalar: felt252, p: NonZeroEcPoint) nopanic {
        ec_state_add_mul(ref self, :scalar, :p);
    }

    /// Finalizes the EC computation and returns the result as a non-zero point.
    ///
    /// # Returns
    ///
    /// * `Option<NonZeroEcPoint>` - The resulting point, or None if the result is the zero point
    ///
    /// # Panics
    ///
    /// Panics if the result is the point at infinity.
    #[inline]
    fn finalize_nz(self: EcState) -> Option<NonZeroEcPoint> nopanic {
        ec_state_try_finalize_nz(self)
    }

    /// Finalizes the EC computation and returns the result.
    ///
    /// Returns the zero point if the computation results in the point at infinity.
    #[inline]
    fn finalize(self: EcState) -> EcPoint {
        match self.finalize_nz() {
            Some(p_nz) => p_nz.into(),
            None => ec_point_zero(),
        }
    }
}

#[generate_trait]
pub impl EcPointImpl of EcPointTrait {
    /// Creates a new EC point from its (x, y) coordinates.
    ///
    /// # Arguments
    ///
    /// * `x` - The x-coordinate of the point
    /// * `y` - The y-coordinate of the point
    ///
    /// # Returns
    ///
    /// Returns `None` if the point (x, y) is not on the curve.
    ///
    /// # Examples
    ///
    /// ```
    /// let point = EcPointTrait::new(
    ///     x: 336742005567258698661916498343089167447076063081786685068305785816009957563,
    ///     y: 1706004133033694959518200210163451614294041810778629639790706933324248611779,
    /// ).unwrap();
    /// ```
    #[inline]
    fn new(x: felt252, y: felt252) -> Option<EcPoint> {
        Some(Self::new_nz(:x, :y)?.into())
    }

    /// Creates a new NonZero EC point from its (x, y) coordinates.
    #[inline]
    fn new_nz(x: felt252, y: felt252) -> Option<NonZeroEcPoint> {
        ec_point_try_new_nz(:x, :y)
    }

    /// Creates a new EC point from its x coordinate.
    ///
    /// # Arguments
    ///
    /// * `x` - The x-coordinate of the point
    ///
    /// # Returns
    ///
    /// Returns `None` if no point with the given x-coordinate exists on the curve.
    ///
    /// # Panics
    ///
    /// Panics if `x` is 0, as this would be the point at infinity.
    ///
    /// # Examples
    ///
    /// ```
    /// let valid = EcPointTrait::new_from_x(1);
    /// assert!(valid.is_some());
    /// let invalid = EcPointTrait::new_from_x(0);
    /// assert!(invalid.is_none());
    /// ```
    #[inline]
    fn new_from_x(x: felt252) -> Option<EcPoint> {
        Some(Self::new_nz_from_x(:x)?.into())
    }

    /// Creates a new NonZero EC point from its x coordinate.
    #[inline]
    fn new_nz_from_x(x: felt252) -> Option<NonZeroEcPoint> {
        ec_point_from_x_nz(:x)
    }


    /// Returns the coordinates of the EC point.
    ///
    /// # Returns
    ///
    /// A tuple containing the (x, y) coordinates of the point.
    ///
    /// # Panics
    ///
    /// Panics if the point is the point at infinity.
    ///
    /// # Examples
    ///
    /// ```
    /// let point_nz = EcPointTrait::new_nz_from_x(1).unwrap();
    /// let (x, _y) = point_nz.coordinates();
    /// assert!(x == 1);
    /// ```
    fn coordinates(self: NonZeroEcPoint) -> (felt252, felt252) {
        ec_point_unwrap(self)
    }

    /// Returns the x coordinate of the EC point.
    ///
    /// # Panics
    ///
    /// Panics if the point is the point at infinity.
    ///
    /// # Examples
    ///
    /// ```
    /// let point_nz = EcPointTrait::new_nz_from_x(1).unwrap();
    /// let x = point_nz.x();
    /// assert!(x == 1);
    /// ```
    fn x(self: NonZeroEcPoint) -> felt252 {
        let (x, _) = self.coordinates();
        x
    }

    /// Returns the y coordinate of the EC point.
    ///
    /// # Panics
    ///
    /// Panics if the point is the point at infinity.
    ///
    /// # Examples
    ///
    /// ```
    /// let gen_point =
    /// EcPointTrait::new_nz_from_x(0x1ef15c18599971b7beced415a40f0c7deacfd9b0d1819e03d723d8bc943cfca).unwrap();
    /// let y = gen_point.y();
    /// assert!(y == 0x5668060aa49730b7be4801df46ec62de53ecd11abe43a32873000c36e8dc1f);
    /// ```
    fn y(self: NonZeroEcPoint) -> felt252 {
        let (_, y) = self.coordinates();
        y
    }

    /// Computes the product of an EC point by the given scalar.
    ///
    /// # Arguments
    ///
    /// * `scalar` - The scalar to multiply the point by
    ///
    /// # Returns
    ///
    /// The resulting point after scalar multiplication.
    fn mul(self: EcPoint, scalar: felt252) -> EcPoint {
        match self.try_into() {
            Some(self_nz) => {
                let mut state = EcStateTrait::init();
                state.add_mul(scalar, self_nz);
                state.finalize()
            },
            None => self,
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
            Some(pt) => pt,
            None => { return rhs; },
        };
        let rhs_nz = match rhs.try_into() {
            Some(pt) => pt,
            None => { return lhs; },
        };
        let mut state = ec_state_init();
        state.add(lhs_nz);
        state.add(rhs_nz);

        state.finalize()
    }
}

#[feature("deprecated-op-assign-traits")]
impl EcPointAddEq of crate::traits::AddEq<EcPoint> {
    #[inline]
    fn add_eq(ref self: EcPoint, other: EcPoint) {
        self = Add::add(self, other);
    }
}

impl EcPointSub of Sub<EcPoint> {
    /// Computes the difference between two points on the curve.
    fn sub(lhs: EcPoint, rhs: EcPoint) -> EcPoint {
        let nz_point: Option<NonZero<EcPoint>> = rhs.try_into();
        if nz_point.is_none() {
            // lhs - 0 = lhs.
            return lhs;
        }
        // lhs - rhs = lhs + (-rhs).
        lhs + (-rhs)
    }
}

#[feature("deprecated-op-assign-traits")]
impl EcPointSubEq of crate::traits::SubEq<EcPoint> {
    #[inline]
    fn sub_eq(ref self: EcPoint, other: EcPoint) {
        self = Sub::sub(self, other);
    }
}
