use std::marker::PhantomData;

use super::interoperability::ContractAddressType;
use super::syscalls::SystemType;
use crate::extensions::array::ArrayType;
use crate::extensions::boxing::BoxType;
use crate::extensions::felt::FeltType;
use crate::extensions::gas::GasBuiltinType;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::snapshot::SnapshotType;
use crate::extensions::structure::StructType;
use crate::extensions::uint::Uint64Type;
use crate::extensions::uint128::Uint128Type;
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::{ConcreteTypeId, UserTypeId};
use crate::program::GenericArg;

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

/// Helper for getting a boxed type.

/// Helper for getting a boxed type for a given type `T`.
pub fn boxed_ty(
    context: &dyn SignatureSpecializationContext,
    ty: ConcreteTypeId,
) -> Result<ConcreteTypeId, SpecializationError> {
    context.get_wrapped_concrete_type(BoxType::id(), ty)
}

/// Helper for ExecutionInfo type def.
fn get_execution_info_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    let contract_address_ty = context.get_concrete_type(ContractAddressType::id(), &[])?;
    context.get_concrete_type(
        StructType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string("core::starknet::info::ExecutionInfo")),
            // block_info
            GenericArg::Type(boxed_ty(context, get_block_info_type(context)?)?),
            // tx_info
            GenericArg::Type(boxed_ty(context, get_tx_info_type(context)?)?),
            // caller_address
            GenericArg::Type(contract_address_ty.clone()),
            // contract_address
            GenericArg::Type(contract_address_ty),
        ],
    )
}

/// Helper for BlockInfo type def.
fn get_block_info_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    let contract_address_ty = context.get_concrete_type(ContractAddressType::id(), &[])?;
    let u64_ty = context.get_concrete_type(Uint64Type::id(), &[])?;
    context.get_concrete_type(
        StructType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string("core::starknet::info::BlockInfo")),
            // block_number
            GenericArg::Type(u64_ty.clone()),
            // block_timestamp
            GenericArg::Type(u64_ty),
            // sequencer_address
            GenericArg::Type(contract_address_ty),
        ],
    )
}

/// Helper for TxInfo type def.
fn get_tx_info_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<ConcreteTypeId, SpecializationError> {
    let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
    let contract_address_ty = context.get_concrete_type(ContractAddressType::id(), &[])?;
    let u128_ty = context.get_concrete_type(Uint128Type::id(), &[])?;
    let felt_array_ty = context.get_wrapped_concrete_type(ArrayType::id(), felt_ty.clone())?;
    let felt_array_snapshot_ty =
        context.get_wrapped_concrete_type(SnapshotType::id(), felt_array_ty)?;
    let felt_array_span_ty = context.get_concrete_type(
        StructType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string("core::array::Span::<core::felt>")),
            GenericArg::Type(felt_array_snapshot_ty),
        ],
    )?;
    context.get_concrete_type(
        StructType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string("core::starknet::info::TxInfo")),
            // version
            GenericArg::Type(felt_ty.clone()),
            // account_contract_address
            GenericArg::Type(contract_address_ty),
            // max_fee
            GenericArg::Type(u128_ty),
            // signature
            GenericArg::Type(felt_array_span_ty),
            // transaction_hash
            GenericArg::Type(felt_ty.clone()),
            // chain_id
            GenericArg::Type(felt_ty.clone()),
            // nonce
            GenericArg::Type(felt_ty),
        ],
    )
}

#[derive(Default)]
pub struct GetExecutionInfoTrait {}
impl GetterTraitsEx for GetExecutionInfoTrait {
    const STR_ID: &'static str = "get_execution_info_syscall";

    fn info_type_id(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<ConcreteTypeId, SpecializationError> {
        boxed_ty(context, get_execution_info_type(context)?)
    }
}
