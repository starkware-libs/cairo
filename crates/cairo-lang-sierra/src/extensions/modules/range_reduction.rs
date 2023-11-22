use cairo_felt::Felt252;
use num_bigint::BigInt;
use num_traits::Zero;

use super::felt252::Felt252Type;
use super::felt252_bounded::Felt252BoundedType;
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

define_libfunc_hierarchy! {
    pub enum Felt252BoundedLibfunc {
        ConstrainRange(Felt252BoundedConstrainRangeLibfunc),
    }, Felt252BoundedConcreteLibfunc
}

pub struct Felt252BoundedConstrainRange {
    pub signature: LibfuncSignature,
    /// The range bounds for a value of the input.
    pub in_range: Range,
    /// The range bounds for a value of the outpus.
    pub out_range: Range,
}
impl SignatureBasedConcreteLibfunc for Felt252BoundedConstrainRange {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}
fn extract_bounds(ty_info: &TypeInfo) -> Result<Range, SpecializationError> {
    return if ty_info.long_id.generic_id == Felt252BoundedType::id() {
        let [GenericArg::Value(lower), GenericArg::Value(upper)] =
            ty_info.long_id.generic_args.as_slice()
        else {
            return Err(SpecializationError::UnsupportedGenericArg);
        };
        Ok(Range { lower: lower.clone(), upper: upper.clone() + 1 })
    } else if ty_info.long_id.generic_id == Felt252Type::id() {
        Ok(Range { lower: 0.into(), upper: Felt252::prime().into() })
    } else {
        return Err(SpecializationError::UnsupportedGenericArg);
    };
}
#[derive(Default)]

pub struct Felt252BoundedConstrainRangeLibfunc {}
impl Felt252BoundedConstrainRangeLibfunc {
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
        // Only allowing `upper_bound <= 2**128` for cheap checking for when in range.
        // TODO(tomerstarkware): support more ranges.
        if out_range.upper > BigInt::from(u128::MAX) + 1 {
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
impl NamedLibfunc for Felt252BoundedConstrainRangeLibfunc {
    type Concrete = Felt252BoundedConstrainRange;
    const STR_ID: &'static str = "felt252_bounded_constrain_range";

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
