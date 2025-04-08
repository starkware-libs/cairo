use num_traits::Zero;
use starknet_types_core::felt::Felt as Felt252;

use super::range_check::RangeCheckType;
use super::utils::{Range, reinterpret_cast_signature};
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureOnlyGenericLibfunc, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    NamedLibfunc, NamedType, OutputVarReferenceInfo, SignatureBasedConcreteLibfunc,
    SpecializationError, args_as_two_types,
};
use crate::ids::ConcreteTypeId;
use crate::program::GenericArg;

define_libfunc_hierarchy! {
    pub enum CastLibfunc {
        Downcast(DowncastLibfunc),
        Upcast(UpcastLibfunc),
    }, CastConcreteLibfunc
}

/// The type of casting between two integer types.
#[derive(PartialEq, Eq)]
pub struct CastType {
    /// Does the source type have values above the destination type possible values.
    pub overflow_above: bool,
    /// Does the source type have values below the destination type possible values.
    pub overflow_below: bool,
}

/// Libfunc for casting from one type to another where any input value can fit into the destination
/// type. For example, from u8 to u64.
#[derive(Default)]
pub struct UpcastLibfunc {}
impl SignatureOnlyGenericLibfunc for UpcastLibfunc {
    const STR_ID: &'static str = "upcast";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let (from_ty, to_ty) = args_as_two_types(args)?;
        let from_range = Range::from_type(context, from_ty.clone())?;
        let to_range: Range = Range::from_type(context, to_ty.clone())?;
        let is_upcast = to_range.lower <= from_range.lower && from_range.upper <= to_range.upper;
        if !is_upcast {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        Ok(reinterpret_cast_signature(from_ty, to_ty))
    }
}

/// A concrete version of the `downcast` libfunc. See [DowncastLibfunc].
pub struct DowncastConcreteLibfunc {
    pub signature: LibfuncSignature,
    pub from_ty: ConcreteTypeId,
    pub from_range: Range,
    pub to_ty: ConcreteTypeId,
    pub to_range: Range,
}
impl DowncastConcreteLibfunc {
    /// Returns the cast type.
    pub fn cast_type(&self) -> CastType {
        if self.from_ty == self.to_ty && self.from_range.lower.is_zero() {
            // Backwards compatibility for the case of casting an unsigned type to itself.
            CastType { overflow_above: true, overflow_below: false }
        } else {
            CastType {
                overflow_above: self.to_range.upper < self.from_range.upper,
                overflow_below: self.to_range.lower > self.from_range.lower,
            }
        }
    }
}

impl SignatureBasedConcreteLibfunc for DowncastConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Libfunc for casting from one type to another where the input value may not fit into the
/// destination type. For example, from u64 to u8.
#[derive(Default)]
pub struct DowncastLibfunc {}
impl NamedLibfunc for DowncastLibfunc {
    type Concrete = DowncastConcreteLibfunc;
    const STR_ID: &'static str = "downcast";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let (from_ty, to_ty) = args_as_two_types(args)?;
        let to_range = Range::from_type(context, to_ty.clone())?;
        let from_range = Range::from_type(context, from_ty.clone())?;
        // Shrinking the range of the destination type by the range of the source type.
        // This is necessary for example in the case `[0, PRIME) -> i8`.
        // In this case `PRIME - 1` is a valid value in `from_range` and it is equivalent
        // to `-1` in the field. Yet, we must make sure `PRIME - 1` is not downcasted to `-1`.
        // By reducing `to_range`, we get a cast `[0, PRIME) -> [0, 128)` where `-1` is not
        // in the output range.
        //
        // Note that the call to `intersection` additionally disallows disjoint ranges.
        let to_range =
            to_range.intersection(&from_range).ok_or(SpecializationError::UnsupportedGenericArg)?;

        // Currently, we only support downcasting into `RangeCheck` sized types.
        if !to_range.is_small_range() {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        let is_small_values_downcast = from_range.is_small_range();
        // Only allow `size < prime % u128::MAX` so that we can safely use `K=2` in
        // `validate_under_limit`.
        let is_felt252_valid_downcast = from_range.is_full_felt252_range()
            && to_range.size() < (Felt252::prime() % u128::MAX).into();
        if !(is_small_values_downcast || is_felt252_valid_downcast) {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let rc_output_info = OutputVarInfo::new_builtin(range_check_type.clone(), 0);
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(range_check_type).with_allow_add_const(),
                ParamSignature::new(from_ty),
            ],
            branch_signatures: vec![
                // Success.
                BranchSignature {
                    vars: vec![
                        rc_output_info.clone(),
                        OutputVarInfo {
                            ty: to_ty,
                            ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 1 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure.
                BranchSignature {
                    vars: vec![rc_output_info],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let (from_ty, to_ty) = args_as_two_types(args)?;
        let from_range = Range::from_type(context.upcast(), from_ty.clone())?;
        // Shrinking the range of the destination type by the range of the source type.
        let to_range: Range = Range::from_type(context.upcast(), to_ty.clone())?
            .intersection(&from_range)
            .ok_or(SpecializationError::UnsupportedGenericArg)?;
        Ok(DowncastConcreteLibfunc {
            signature: self.specialize_signature(context.upcast(), args)?,
            from_range,
            from_ty,
            to_range,
            to_ty,
        })
    }
}
