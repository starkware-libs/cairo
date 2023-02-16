use std::marker::PhantomData;

use super::interoperability::ContractAddressType;
use super::syscalls::SystemType;
use crate::extensions::array::ArrayType;
use crate::extensions::felt::FeltType;
use crate::extensions::gas::GasBuiltinType;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::uint::Uint64Type;
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::ConcreteTypeId;

/// Trait for implementing getters.
pub trait GetterTraits: Default {
    /// The generic libfunc id for the getter libfunc.
    const STR_ID: &'static str;
    /// The simple sierra generic type returned by the getter.
    type InfoType: NoGenericArgsGenericType;
}

/// Same as GetterTraits, but with a function to return the concrete TypeId.
pub trait GetterTraitsEx: Default {
    /// The generic libfunc id for the getter libfunc.
    const STR_ID: &'static str;
    /// The simple sierra generic type returned by the getter.
    fn info_type_id(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<ConcreteTypeId, SpecializationError>;
}

impl<TGetterTraits: GetterTraits> GetterTraitsEx for TGetterTraits {
    const STR_ID: &'static str = TGetterTraits::STR_ID;
    fn info_type_id(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<ConcreteTypeId, SpecializationError> {
        context.get_concrete_type(TGetterTraits::InfoType::id(), &[])
    }
}

/// Libfunc for a getter system call.
#[derive(Default)]
pub struct GetterLibfunc<TGetterTraitsEx: GetterTraitsEx> {
    _phantom: PhantomData<TGetterTraitsEx>,
}
impl<TGetterTraitsEx: GetterTraitsEx> NoGenericArgsGenericLibfunc
    for GetterLibfunc<TGetterTraitsEx>
{
    const STR_ID: &'static str = TGetterTraitsEx::STR_ID;
    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        let info_ty = TGetterTraitsEx::info_type_id(context)?;
        let gas_builtin_ty = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        let system_ty = context.get_concrete_type(SystemType::id(), &[])?;
        let felt_array_ty = context.get_wrapped_concrete_type(ArrayType::id(), felt_ty)?;
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

#[derive(Default)]
pub struct GetCallerAddressTrait {}
impl GetterTraits for GetCallerAddressTrait {
    const STR_ID: &'static str = "get_caller_address_syscall";
    type InfoType = ContractAddressType;
}

#[derive(Default)]
pub struct GetContractAddressTrait {}
impl GetterTraits for GetContractAddressTrait {
    const STR_ID: &'static str = "get_contract_address_syscall";
    type InfoType = ContractAddressType;
}

#[derive(Default)]
pub struct GetSequencerAddressTrait {}
impl GetterTraits for GetSequencerAddressTrait {
    const STR_ID: &'static str = "get_sequencer_address_syscall";
    type InfoType = ContractAddressType;
}

#[derive(Default)]
pub struct GetBlockNumberTrait {}
impl GetterTraits for GetBlockNumberTrait {
    const STR_ID: &'static str = "get_block_number_syscall";
    type InfoType = Uint64Type;
}

#[derive(Default)]
pub struct GetBlockTimestampTrait {}
impl GetterTraits for GetBlockTimestampTrait {
    const STR_ID: &'static str = "get_block_timestamp_syscall";
    type InfoType = Uint64Type;
}

#[derive(Default)]
pub struct GetTxInfoTrait {}
impl GetterTraitsEx for GetTxInfoTrait {
    const STR_ID: &'static str = "get_tx_info_syscall";

    fn info_type_id(
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<ConcreteTypeId, SpecializationError> {
        todo!();
    }
}
