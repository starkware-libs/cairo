use std::marker::PhantomData;

use super::syscalls::SystemType;
use crate::extensions::felt::FeltType;
use crate::extensions::gas::GasBuiltinType;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};

/// Trait for implementing unsigned integers.
pub trait GetterTraits: Default {
    /// The generic libfunc id for the getter libfunc.
    const STR_ID: &'static str;
    /// The simple sierra generic type returned by the getter.
    type InfoType: NoGenericArgsGenericType;
}

/// Libfunc for an emit event system call.
#[derive(Default)]
pub struct GetterLibfunc<TGetterTraits: GetterTraits> {
    _phantom: PhantomData<TGetterTraits>,
}
impl<TGetterTraits: GetterTraits> NoGenericArgsGenericLibfunc for GetterLibfunc<TGetterTraits> {
    const STR_ID: &'static str = TGetterTraits::STR_ID;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        let info_ty = context.get_concrete_type(TGetterTraits::InfoType::id(), &[])?;
        let gas_builtin_ty = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        let system_ty = context.get_concrete_type(SystemType::id(), &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // Gas builtin
                ParamSignature::new(gas_builtin_ty.clone()),
                // System
                ParamSignature {
                    ty: system_ty.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
            ],
            branch_signatures: vec![
                // Success branch.
                BranchSignature {
                    vars: vec![
                        // Gas builtin
                        OutputVarInfo {
                            ty: gas_builtin_ty.clone(),
                            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                        },
                        // System
                        OutputVarInfo {
                            ty: system_ty.clone(),
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 1 },
                            ),
                        },
                        // Returned information
                        OutputVarInfo {
                            ty: info_ty,
                            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure branch.
                BranchSignature {
                    vars: vec![
                        // Gas builtin
                        OutputVarInfo {
                            ty: gas_builtin_ty,
                            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                        },
                        // System
                        OutputVarInfo {
                            ty: system_ty,
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 1 },
                            ),
                        },
                        // Revert reason
                        OutputVarInfo {
                            ty: felt_ty,
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

#[derive(Default)]
pub struct GetCallerAddressTrait {}
impl GetterTraits for GetCallerAddressTrait {
    const STR_ID: &'static str = "get_caller_address";
    type InfoType = FeltType;
}
