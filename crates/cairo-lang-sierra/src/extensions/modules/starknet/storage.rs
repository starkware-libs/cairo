use super::syscalls::SystemType;
use crate::extensions::consts::{ConstGenLibfunc, WrapConstGenLibfunc};
use crate::extensions::felt::FeltType;
use crate::extensions::gas::GasBuiltinType;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::range_check::RangeCheckType;
use crate::extensions::uint::Uint8Type;
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::GenericTypeId;

/// Type for StarkNet storage base address, a value in the range [0, 2 ** 251 - 256).
#[derive(Default)]
pub struct StorageBaseAddressType {}
impl NoGenericArgsGenericType for StorageBaseAddressType {
    const ID: GenericTypeId = GenericTypeId::new_inline("StorageBaseAddress");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const SIZE: i16 = 1;
}

/// Libfunc for creating a constant storage base address.
#[derive(Default)]
pub struct StorageBaseAddressConstLibfuncWrapped {}
impl ConstGenLibfunc for StorageBaseAddressConstLibfuncWrapped {
    const STR_ID: &'static str = ("storage_base_address_const");
    const GENERIC_TYPE_ID: GenericTypeId = <StorageBaseAddressType as NoGenericArgsGenericType>::ID;
}

pub type StorageBaseAddressConstLibfunc =
    WrapConstGenLibfunc<StorageBaseAddressConstLibfuncWrapped>;

/// Type for StarkNet storage base address, a value in the range [0, 2 ** 251).
#[derive(Default)]
pub struct StorageAddressType {}
impl NoGenericArgsGenericType for StorageAddressType {
    const ID: GenericTypeId = GenericTypeId::new_inline("StorageAddress");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const SIZE: i16 = 1;
}

/// Libfunc for converting a base address into a storage address.
#[derive(Default)]
pub struct StorageAddressFromBaseLibfunc {}
impl NoGenericArgsGenericLibfunc for StorageAddressFromBaseLibfunc {
    const STR_ID: &'static str = "storage_address_from_base";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature {
                ty: context.get_concrete_type(StorageBaseAddressType::id(), &[])?,
                allow_deferred: true,
                allow_add_const: true,
                allow_const: true,
            }],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(StorageAddressType::id(), &[])?,
                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for converting a base address and offset into a storage address.
#[derive(Default)]
pub struct StorageAddressFromBaseAndOffsetLibfunc {}
impl NoGenericArgsGenericLibfunc for StorageAddressFromBaseAndOffsetLibfunc {
    const STR_ID: &'static str = "storage_address_from_base_and_offset";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(context.get_concrete_type(StorageBaseAddressType::id(), &[])?),
                ParamSignature {
                    ty: context.get_concrete_type(Uint8Type::id(), &[])?,
                    allow_deferred: false,
                    allow_add_const: false,
                    allow_const: true,
                },
            ],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(StorageAddressType::id(), &[])?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for converting a felt into a storage base address.
#[derive(Default)]
pub struct StorageBaseAddressFromFeltLibfunc {}
impl NoGenericArgsGenericLibfunc for StorageBaseAddressFromFeltLibfunc {
    const STR_ID: &'static str = "storage_base_address_from_felt";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let range_check_ty = context.get_concrete_type(RangeCheckType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature {
                    ty: range_check_ty.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
                ParamSignature::new(context.get_concrete_type(FeltType::id(), &[])?),
            ],
            vec![
                OutputVarInfo {
                    ty: range_check_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                },
                OutputVarInfo {
                    ty: context.get_concrete_type(StorageBaseAddressType::id(), &[])?,
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                },
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}

/// Libfunc for a storage read system call.
#[derive(Default)]
pub struct StorageReadLibfunc {}
impl NoGenericArgsGenericLibfunc for StorageReadLibfunc {
    const STR_ID: &'static str = "storage_read_syscall";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let gas_builtin_ty = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        let system_ty = context.get_concrete_type(SystemType::id(), &[])?;
        let addr_ty = context.get_concrete_type(StorageAddressType::id(), &[])?;
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;

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
                // Address domain
                ParamSignature::new(felt_ty.clone()),
                // Address
                ParamSignature::new(addr_ty),
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
                        // read_value
                        OutputVarInfo {
                            ty: felt_ty.clone(),
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

/// Libfunc for a storage write system call.
#[derive(Default)]
pub struct StorageWriteLibfunc {}
impl NoGenericArgsGenericLibfunc for StorageWriteLibfunc {
    const STR_ID: &'static str = "storage_write_syscall";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let gas_builtin_ty = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        let system_ty = context.get_concrete_type(SystemType::id(), &[])?;
        let addr_ty = context.get_concrete_type(StorageAddressType::id(), &[])?;
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
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
                // Address domain
                ParamSignature::new(felt_ty.clone()),
                // Address
                ParamSignature::new(addr_ty),
                // Value
                ParamSignature::new(felt_ty.clone()),
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
