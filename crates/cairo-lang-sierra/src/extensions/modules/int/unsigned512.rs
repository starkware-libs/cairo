use super::unsigned128::{U128MulGuaranteeType, Uint128Type};
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureSpecializationContext,
};
use crate::extensions::modules::get_u256_type;
use crate::extensions::non_zero::nonzero_ty;
use crate::extensions::range_check::RangeCheckType;
use crate::extensions::structure::StructType;
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::{ConcreteTypeId, UserTypeId};
use crate::program::GenericArg;

define_libfunc_hierarchy! {
    pub enum Uint512Libfunc {
        DivModU256(Uint512DivmodU256Libfunc),
    }, Uint512Concrete
}

// Divmod.
#[derive(Default)]
pub struct Uint512DivmodU256Libfunc;
impl NoGenericArgsGenericLibfunc for Uint512DivmodU256Libfunc {
    const STR_ID: &'static str = "u512_safe_divmod_by_u256";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let u256_ty = get_u256_type(context)?;
        let u512_ty = get_u512_type(context)?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let guarantee_ty = context.get_concrete_type(U128MulGuaranteeType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(range_check_type.clone()).with_allow_add_const(),
                ParamSignature::new(u512_ty.clone()),
                ParamSignature::new(nonzero_ty(context, &u256_ty)?),
            ],
            vec![
                OutputVarInfo {
                    ty: range_check_type,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                },
                OutputVarInfo { ty: u512_ty, ref_info: OutputVarReferenceInfo::SimpleDerefs },
                OutputVarInfo { ty: u256_ty, ref_info: OutputVarReferenceInfo::SimpleDerefs },
                OutputVarInfo {
                    ty: guarantee_ty.clone(),
                    ref_info: OutputVarReferenceInfo::SimpleDerefs,
                },
                OutputVarInfo {
                    ty: guarantee_ty.clone(),
                    ref_info: OutputVarReferenceInfo::SimpleDerefs,
                },
                OutputVarInfo {
                    ty: guarantee_ty.clone(),
                    ref_info: OutputVarReferenceInfo::SimpleDerefs,
                },
                OutputVarInfo {
                    ty: guarantee_ty.clone(),
                    ref_info: OutputVarReferenceInfo::SimpleDerefs,
                },
                OutputVarInfo { ty: guarantee_ty, ref_info: OutputVarReferenceInfo::SimpleDerefs },
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Helper for u512 type def.
fn get_u512_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    let u128_ty = context.get_concrete_type(Uint128Type::id(), &[])?;
    context.get_concrete_type(
        StructType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string("core::integer::u512")),
            GenericArg::Type(u128_ty.clone()),
            GenericArg::Type(u128_ty.clone()),
            GenericArg::Type(u128_ty.clone()),
            GenericArg::Type(u128_ty),
        ],
    )
}
