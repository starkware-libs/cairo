use std::ops::Shl;

use cairo_felt::Felt252;
use num_bigint::{BigInt, ToBigInt};
use num_traits::{One, Zero};

use super::bytes31::Bytes31Type;
use super::felt252::Felt252Type;
use super::felt252_bounded::Felt252BoundedType;
use super::int::signed::{Sint16Type, Sint32Type, Sint64Type, Sint8Type};
use super::int::signed128::Sint128Type;
use super::int::unsigned::{Uint16Type, Uint32Type, Uint64Type, Uint8Type};
use super::int::unsigned128::Uint128Type;
use super::range_check::RangeCheckType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    NamedLibfunc, NamedType, OutputVarReferenceInfo, SignatureBasedConcreteLibfunc,
    SpecializationError,
};
use crate::program::GenericArg;

/// An half-close half-open  range.
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

define_libfunc_hierarchy! {
    pub enum RangeLibfunc {
        ConstrainRange(ConstrainRangeLibfunc),
    }, RangeConcreteLibfunc
}

pub struct Felt252BoundedConstrainRange {
    pub signature: LibfuncSignature,
    /// The range bounds for a value of the input.
    pub in_range: Range,
    /// The range bounds for a value of the outputs.
    pub out_range: Range,
}
impl SignatureBasedConcreteLibfunc for Felt252BoundedConstrainRange {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Returns the Range bounds from the given type info.
fn extract_bounds(ty_info: &TypeInfo) -> Result<Range, SpecializationError> {
    Ok(match (&ty_info.long_id.generic_id, &ty_info.long_id.generic_args[..]) {
        (id, []) if *id == Felt252Type::id() => Range::half_open(0.into(), Felt252::prime().into()),
        (id, []) if *id == Uint8Type::id() => Range::closed(u8::MIN.into(), u8::MAX.into()),
        (id, []) if *id == Uint16Type::id() => Range::closed(u16::MIN.into(), u16::MAX.into()),
        (id, []) if *id == Uint32Type::id() => Range::closed(u32::MIN.into(), u32::MAX.into()),
        (id, []) if *id == Uint64Type::id() => Range::closed(u64::MIN.into(), u64::MAX.into()),
        (id, []) if *id == Uint128Type::id() => Range::closed(u128::MIN.into(), u128::MAX.into()),
        (id, []) if *id == Uint16Type::id() => Range::closed(u16::MIN.into(), u16::MAX.into()),
        (id, []) if *id == Sint8Type::id() => Range::closed(i8::MIN.into(), i8::MAX.into()),
        (id, []) if *id == Sint16Type::id() => Range::closed(i16::MIN.into(), i16::MAX.into()),
        (id, []) if *id == Sint32Type::id() => Range::closed(i32::MIN.into(), i32::MAX.into()),
        (id, []) if *id == Sint64Type::id() => Range::closed(i64::MIN.into(), i64::MAX.into()),
        (id, []) if *id == Sint128Type::id() => Range::closed(i128::MIN.into(), i128::MAX.into()),
        (id, []) if *id == Bytes31Type::id() => Range::half_open(0.into(), BigInt::one().shl(248)),
        (id, [GenericArg::Value(lower), GenericArg::Value(upper)])
            if *id == Felt252BoundedType::id() =>
        {
            Range::closed(lower.clone(), upper.clone())
        }
        _ => return Err(SpecializationError::UnsupportedGenericArg),
    })
}
#[derive(Default)]

pub struct ConstrainRangeLibfunc {}
impl ConstrainRangeLibfunc {
    /// Creates the specialization of the Felt252BoundedConstrainRange libfunc with the given
    /// template arguments.
    fn specialize_concrete_lib_func(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Felt252BoundedConstrainRange, SpecializationError> {
        let (in_ty, out_ty) = match args {
            [GenericArg::Type(in_ty), GenericArg::Type(out_ty)] => (in_ty, out_ty),
            [_, _] => return Err(SpecializationError::UnsupportedGenericArg),
            _ => return Err(SpecializationError::WrongNumberOfGenericArgs),
        };

        let in_ty_info = context.get_type_info(in_ty.clone())?;
        let out_ty_info = context.get_type_info(out_ty.clone())?;

        if out_ty_info.long_id.generic_id != Felt252BoundedType::id() {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        let in_range = extract_bounds(&in_ty_info)?;
        let out_range = extract_bounds(&out_ty_info)?;

        if !in_range.lower.is_zero() || !out_range.lower.is_zero() {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        if in_range.lower >= in_range.upper || out_range.lower >= out_range.upper {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        // Only allowing `prime % 2**128` for using `K=2` in the validate_under_limit function.
        // TODO(tomerstarkware): support more ranges.
        if out_range.upper > Felt252::prime().to_bigint().unwrap() % (u128::MAX) {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        if in_range.upper <= out_range.upper {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;

        Ok(Felt252BoundedConstrainRange {
            signature: LibfuncSignature {
                param_signatures: vec![
                    ParamSignature::new(range_check_type.clone()).with_allow_add_const(),
                    ParamSignature::new(in_ty.clone()),
                ],
                branch_signatures: vec![
                    BranchSignature {
                        vars: vec![
                            OutputVarInfo {
                                ty: range_check_type.clone(),
                                ref_info: OutputVarReferenceInfo::Deferred(
                                    DeferredOutputKind::AddConst { param_idx: 0 },
                                ),
                            },
                            OutputVarInfo {
                                ty: out_ty.clone(),
                                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 1 },
                            },
                        ],
                        ap_change: SierraApChange::Known { new_vars_only: false },
                    },
                    BranchSignature {
                        vars: vec![OutputVarInfo {
                            ty: range_check_type,
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        }],
                        ap_change: SierraApChange::Known { new_vars_only: false },
                    },
                ],
                fallthrough: Some(0),
            },
            in_range,
            out_range,
        })
    }
}
impl NamedLibfunc for ConstrainRangeLibfunc {
    type Concrete = Felt252BoundedConstrainRange;
    const STR_ID: &'static str = "constrain_range";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(self.specialize_concrete_lib_func(context, args)?.signature)
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        self.specialize_concrete_lib_func(context.upcast(), args)
    }
}
