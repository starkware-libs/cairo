// Module providing the gas related extensions.
use super::range_check::RangeCheckType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::GenericTypeId;

/// Type for gas actions.
#[derive(Default)]
pub struct GasBuiltinType {}
impl NoGenericArgsGenericType for GasBuiltinType {
    const ID: GenericTypeId = GenericTypeId::new_inline("GasBuiltin");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const SIZE: i16 = 1;
}

define_libfunc_hierarchy! {
    pub enum GasLibfunc {
        GetGas(GetGasLibfunc),
        RefundGas(RefundGasLibfunc),
    }, GasConcreteLibfunc
}

/// Libfunc for getting gas branch.
#[derive(Default)]
pub struct GetGasLibfunc {}
impl NoGenericArgsGenericLibfunc for GetGasLibfunc {
    const STR_ID: &'static str = "get_gas";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let gas_builtin_type = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature {
                    ty: range_check_type.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
                ParamSignature::new(gas_builtin_type.clone()),
            ],
            branch_signatures: vec![
                // Success:
                BranchSignature {
                    vars: vec![
                        OutputVarInfo {
                            ty: range_check_type.clone(),
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        },
                        OutputVarInfo {
                            ty: gas_builtin_type.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure:
                BranchSignature {
                    vars: vec![
                        OutputVarInfo {
                            ty: range_check_type,
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        },
                        OutputVarInfo {
                            ty: gas_builtin_type,
                            ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 1 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// Libfunc for returning unused gas.
#[derive(Default)]
pub struct RefundGasLibfunc {}
impl NoGenericArgsGenericLibfunc for RefundGasLibfunc {
    const STR_ID: &'static str = "refund_gas";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let gas_builtin_type = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch(
            vec![gas_builtin_type.clone()],
            vec![OutputVarInfo {
                ty: gas_builtin_type,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
