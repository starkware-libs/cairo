use super::unsigned128::{U128MulGuaranteeType, Uint128Type};
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureSpecializationContext,
};
use crate::extensions::modules::get_u256_type;
use crate::extensions::non_zero::nonzero_ty;
use crate::extensions::range_check::RangeCheckType;
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, OutputVarReferenceInfo, SpecializationError,
};

define_libfunc_hierarchy! {
    pub enum Uint256Libfunc {
        IsZero(Uint256IsZeroLibfunc),
        Divmod(Uint256DivmodLibfunc),
        SquareRoot(Uint256SquareRootLibfunc),
    }, Uint256Concrete
}

// IsZero.
#[derive(Default)]
pub struct Uint256IsZeroLibfunc;
impl NoGenericArgsGenericLibfunc for Uint256IsZeroLibfunc {
    const STR_ID: &'static str = "u256_is_zero";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let u256_ty = get_u256_type(context)?;
        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(u256_ty.clone())],
            branch_signatures: vec![
                // Zero.
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
                // NonZero.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: nonzero_ty(context, &u256_ty)?,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

// Divmod.
#[derive(Default)]
pub struct Uint256DivmodLibfunc;
impl NoGenericArgsGenericLibfunc for Uint256DivmodLibfunc {
    const STR_ID: &'static str = "u256_safe_divmod";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let u256_type = get_u256_type(context)?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let simple_deref_u256_output_info =
            OutputVarInfo { ty: u256_type.clone(), ref_info: OutputVarReferenceInfo::SimpleDerefs };
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(range_check_type.clone()).with_allow_add_const(),
                ParamSignature::new(u256_type.clone()),
                ParamSignature::new(nonzero_ty(context, &u256_type)?),
            ],
            vec![
                OutputVarInfo::new_builtin(range_check_type, 0),
                simple_deref_u256_output_info.clone(),
                simple_deref_u256_output_info,
                OutputVarInfo {
                    ty: context.get_concrete_type(U128MulGuaranteeType::id(), &[])?,
                    ref_info: OutputVarReferenceInfo::SimpleDerefs,
                },
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

// Square root.
#[derive(Default)]
pub struct Uint256SquareRootLibfunc;
impl NoGenericArgsGenericLibfunc for Uint256SquareRootLibfunc {
    const STR_ID: &'static str = "u256_sqrt";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(range_check_type.clone()).with_allow_add_const(),
                ParamSignature::new(get_u256_type(context)?),
            ],
            vec![
                OutputVarInfo::new_builtin(range_check_type, 0),
                OutputVarInfo {
                    ty: context.get_concrete_type(Uint128Type::id(), &[])?,
                    ref_info: OutputVarReferenceInfo::SimpleDerefs,
                },
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}
