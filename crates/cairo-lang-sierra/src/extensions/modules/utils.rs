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
    LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
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

/// An half-close half-open range.
pub struct Range {
    /// The lower bound (Inclusive).
    pub lower: BigInt,
    /// The upper bound (Exclusive).
    pub upper: BigInt,
}
impl Range {
    /// Creates a closed range i.e. `[lower, upper]`.
    pub fn closed(lower: BigInt, upper: BigInt) -> Self {
        Self { lower, upper: upper + 1 }
    }
    /// Creates a half-closed range i.e. `[lower, upper)`.
    pub fn half_open(lower: BigInt, upper: BigInt) -> Self {
        Self { lower, upper }
    }
}

/// Returns the Range bounds from the given type info.
pub fn extract_bounds(ty_info: &TypeInfo) -> Result<Range, SpecializationError> {
    Ok(match (&ty_info.long_id.generic_id, &ty_info.long_id.generic_args[..]) {
        (id, []) if *id == Felt252Type::id() => Range::half_open(0.into(), Felt252::prime().into()),
        (id, []) if *id == Uint8Type::id() => Range::closed(u8::MIN.into(), u8::MAX.into()),
        (id, []) if *id == Uint16Type::id() => Range::closed(u16::MIN.into(), u16::MAX.into()),
        (id, []) if *id == Uint32Type::id() => Range::closed(u32::MIN.into(), u32::MAX.into()),
        (id, []) if *id == Uint64Type::id() => Range::closed(u64::MIN.into(), u64::MAX.into()),
        (id, []) if *id == Uint128Type::id() => Range::closed(u128::MIN.into(), u128::MAX.into()),
        (id, []) if *id == Sint8Type::id() => Range::closed(i8::MIN.into(), i8::MAX.into()),
        (id, []) if *id == Sint16Type::id() => Range::closed(i16::MIN.into(), i16::MAX.into()),
        (id, []) if *id == Sint32Type::id() => Range::closed(i32::MIN.into(), i32::MAX.into()),
        (id, []) if *id == Sint64Type::id() => Range::closed(i64::MIN.into(), i64::MAX.into()),
        (id, []) if *id == Sint128Type::id() => Range::closed(i128::MIN.into(), i128::MAX.into()),
        (id, []) if *id == Bytes31Type::id() => Range::half_open(0.into(), BigInt::one().shl(248)),
        (id, [GenericArg::Value(min), GenericArg::Value(max)]) if *id == BoundedIntType::id() => {
            Range::closed(min.clone(), max.clone())
        }
        _ => return Err(SpecializationError::UnsupportedGenericArg),
    })
}
