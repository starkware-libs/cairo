use super::get_bool_type;
use crate::define_libfunc_hierarchy;
use crate::extensions::felt252::Felt252Type;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, OutputVarReferenceInfo, SpecializationError,
};

define_libfunc_hierarchy! {
    pub enum BoolLibfunc {
        And(BoolAndLibfunc),
        Not(BoolNotLibfunc),
        Xor(BoolXorLibfunc),
        Or(BoolOrLibfunc),
        Equal(BoolEqualLibfunc),
        ToFelt252(BoolToFelt252Libfunc),
    }, BoolConcreteLibfunc
}

/// Libfunc for converting a bool into a felt252.
#[derive(Default)]
pub struct BoolToFelt252Libfunc {}
impl NoGenericArgsGenericLibfunc for BoolToFelt252Libfunc {
    const STR_ID: &'static str = "bool_to_felt252";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature {
                ty: get_bool_type(context)?,
                allow_deferred: true,
                allow_add_const: true,
                allow_const: true,
            }],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(Felt252Type::id(), &[])?,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Utility for common boolean libfunc signature definitions.
fn boolean_libfunc_signature(
    context: &dyn SignatureSpecializationContext,
    new_vars_only: bool,
    is_unary: bool,
) -> Result<LibfuncSignature, SpecializationError> {
    let bool_type = get_bool_type(context)?;
    Ok(LibfuncSignature::new_non_branch(
        if is_unary { vec![bool_type.clone()] } else { vec![bool_type.clone(), bool_type.clone()] },
        vec![OutputVarInfo {
            ty: bool_type,
            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
        }],
        SierraApChange::Known { new_vars_only },
    ))
}

/// Libfunc for boolean AND.
#[derive(Default)]
pub struct BoolAndLibfunc {}
impl NoGenericArgsGenericLibfunc for BoolAndLibfunc {
    const STR_ID: &'static str = "bool_and_impl";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        boolean_libfunc_signature(context, true, false)
    }
}

/// Libfunc for boolean NOT.
#[derive(Default)]
pub struct BoolNotLibfunc {}
impl NoGenericArgsGenericLibfunc for BoolNotLibfunc {
    const STR_ID: &'static str = "bool_not_impl";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        boolean_libfunc_signature(context, false, true)
    }
}

/// Libfunc for boolean XOR.
#[derive(Default)]
pub struct BoolXorLibfunc {}
impl NoGenericArgsGenericLibfunc for BoolXorLibfunc {
    const STR_ID: &'static str = "bool_xor_impl";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        boolean_libfunc_signature(context, false, false)
    }
}

/// Libfunc for boolean OR.
#[derive(Default)]
pub struct BoolOrLibfunc {}
impl NoGenericArgsGenericLibfunc for BoolOrLibfunc {
    const STR_ID: &'static str = "bool_or_impl";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        boolean_libfunc_signature(context, false, false)
    }
}

/// Libfunc for boolean equality.
#[derive(Default)]
pub struct BoolEqualLibfunc {}
impl NoGenericArgsGenericLibfunc for BoolEqualLibfunc {
    const STR_ID: &'static str = "bool_eq";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let bool_type = get_bool_type(context)?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(bool_type.clone()),
                ParamSignature::new(bool_type).with_allow_const(),
            ],
            branch_signatures: vec![
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
