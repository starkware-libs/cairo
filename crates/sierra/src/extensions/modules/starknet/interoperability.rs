use super::syscalls::SystemType;
use crate::extensions::array::ArrayType;
use crate::extensions::consts::{ConstGenLibFunc, WrapConstGenLibFunc};
use crate::extensions::felt::FeltType;
use crate::extensions::gas::GasBuiltinType;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibFuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibFunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::{GenericLibFuncId, GenericTypeId};
use crate::program::GenericArg;

/// Type for StarkNet storage address, a value in the range [0, 2 ** 250).
#[derive(Default)]
pub struct ContractAddressType {}
impl NoGenericArgsGenericType for ContractAddressType {
    const ID: GenericTypeId = GenericTypeId::new_inline("ContractAddress");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const SIZE: i16 = 1;
}

/// LibFunc for creating a constant storage address.
#[derive(Default)]
pub struct ContractAddressConstLibFuncWrapped {}
impl ConstGenLibFunc for ContractAddressConstLibFuncWrapped {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("contract_address_const");
    const GENERIC_TYPE_ID: GenericTypeId = <ContractAddressType as NoGenericArgsGenericType>::ID;
}

pub type ContractAddressConstLibFunc = WrapConstGenLibFunc<ContractAddressConstLibFuncWrapped>;

/// LibFunc for a storage call contract system call.
#[derive(Default)]
pub struct CallContractLibFunc {}
impl NoGenericArgsGenericLibFunc for CallContractLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("call_contract_syscall");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let gas_builtin_ty = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        let system_ty = context.get_concrete_type(SystemType::id(), &[])?;
        let addr_ty = context.get_concrete_type(ContractAddressType::id(), &[])?;
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        let felt_array_ty =
            context.get_concrete_type(ArrayType::id(), &[GenericArg::Type(felt_ty.clone())])?;
        Ok(LibFuncSignature {
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
                // Address
                ParamSignature::new(addr_ty),
                // Call data
                ParamSignature::new(felt_array_ty.clone()),
            ],
            branch_signatures: vec![
                // Success branch
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
                        // result
                        OutputVarInfo {
                            ty: felt_array_ty.clone(),
                            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
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
                        // result
                        OutputVarInfo {
                            ty: felt_array_ty,
                            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
