use std::ops::Shl;

use cairo_felt::Felt252;
use num_bigint::BigInt;
use num_traits::One;

use super::bounded_int::BoundedIntType;
use super::bytes31::Bytes31Type;
use super::felt252::Felt252Type;
use super::int::signed::{Sint16Type, Sint32Type, Sint64Type, Sint8Type};
use super::int::signed128::Sint128Type;
use super::int::unsigned::{Uint16Type, Uint32Type, Uint64Type, Uint8Type};
use super::int::unsigned128::Uint128Type;
use crate::extensions::lib_func::{
    LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::types::TypeInfo;
use crate::extensions::{NamedType, OutputVarReferenceInfo, SpecializationError};
use crate::ids::ConcreteTypeId;
use crate::program::GenericArg;

/// Returns a libfunc signature that casts from one type to another, without changing the internal
/// representation.
/// The implementation must be the identity function (no CASM output).
pub fn reinterpret_cast_signature(
    from_ty: ConcreteTypeId,
    to_ty: ConcreteTypeId,
) -> LibfuncSignature {
    LibfuncSignature::new_non_branch_ex(
        vec![ParamSignature::new(from_ty).with_allow_all()],
        vec![OutputVarInfo {
            ty: to_ty,
            ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
        }],
        SierraApChange::Known { new_vars_only: true },
    )
}

/// A range of integers (`[lower, upper)`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Range {
    /// The lower bound (Inclusive).
    pub lower: BigInt,
    /// The upper bound (Exclusive).
    pub upper: BigInt,
}
impl Range {
    /// Creates a closed range i.e. `[lower, upper]`.
    pub fn closed(lower: impl Into<BigInt>, upper: impl Into<BigInt>) -> Self {
        Self::half_open(lower, upper.into() as BigInt + 1)
    }
    /// Creates a half-closed range i.e. `[lower, upper)`.
    pub fn half_open(lower: impl Into<BigInt>, upper: impl Into<BigInt>) -> Self {
        let result = Self { lower: lower.into(), upper: upper.into() };
        assert!(result.lower < result.upper, "Invalid range: {:?}", result);
        result
    }
    /// Returns the [Range] bounds from the given type info.
    pub fn from_type_info(ty_info: &TypeInfo) -> Result<Self, SpecializationError> {
        Ok(match (&ty_info.long_id.generic_id, &ty_info.long_id.generic_args[..]) {
            (id, []) if *id == Felt252Type::id() => {
                let prime: BigInt = Felt252::prime().into();
                Self::half_open(1 - &prime, prime)
            }
            (id, []) if *id == Uint8Type::id() => Self::closed(u8::MIN, u8::MAX),
            (id, []) if *id == Uint16Type::id() => Self::closed(u16::MIN, u16::MAX),
            (id, []) if *id == Uint32Type::id() => Self::closed(u32::MIN, u32::MAX),
            (id, []) if *id == Uint64Type::id() => Self::closed(u64::MIN, u64::MAX),
            (id, []) if *id == Uint128Type::id() => Self::closed(u128::MIN, u128::MAX),
            (id, []) if *id == Sint8Type::id() => Self::closed(i8::MIN, i8::MAX),
            (id, []) if *id == Sint16Type::id() => Self::closed(i16::MIN, i16::MAX),
            (id, []) if *id == Sint32Type::id() => Self::closed(i32::MIN, i32::MAX),
            (id, []) if *id == Sint64Type::id() => Self::closed(i64::MIN, i64::MAX),
            (id, []) if *id == Sint128Type::id() => Self::closed(i128::MIN, i128::MAX),
            (id, []) if *id == Bytes31Type::id() => Self::half_open(0, BigInt::one().shl(248)),
            (id, [GenericArg::Value(min), GenericArg::Value(max)])
                if *id == BoundedIntType::id() =>
            {
                Self::closed(min.clone(), max.clone())
            }
            _ => return Err(SpecializationError::UnsupportedGenericArg),
        })
    }
    /// Returns the Range bounds from the given type.
    pub fn from_type(
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<Self, SpecializationError> {
        Self::from_type_info(&context.get_type_info(ty)?)
    }
    /// Returns true if this range is smaller than the RangeCheck range.
    pub fn is_small_range(&self) -> bool {
        self.size() <= BigInt::one().shl(128)
    }
    /// Returns true if this range can contain all possible values of a CASM cell.
    pub fn is_full_felt252_range(&self) -> bool {
        self.size() >= Felt252::prime().into()
    }
    /// Returns the size of the range.
    pub fn size(&self) -> BigInt {
        &self.upper - &self.lower
    }
    /// Returns the intersection of `self` and `other`.
    ///
    /// If the intersection is empty, returns `None`.
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        let lower = std::cmp::max(&self.lower, &other.lower).clone();
        let upper = std::cmp::min(&self.upper, &other.upper).clone();
        if lower < upper { Some(Self::half_open(lower, upper)) } else { None }
    }
}
