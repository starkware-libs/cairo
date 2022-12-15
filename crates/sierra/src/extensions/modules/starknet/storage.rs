use num_bigint::BigInt;

use super::syscalls::SystemType;
use crate::extensions::felt::FeltType;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibFuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::types::{InfoOnlyConcreteType, TypeInfo};
use crate::extensions::{
    NamedLibFunc, NamedType, NoGenericArgsGenericLibFunc, NoGenericArgsGenericType,
    OutputVarReferenceInfo, SignatureBasedConcreteLibFunc, SpecializationError,
};
use crate::ids::{GenericLibFuncId, GenericTypeId};
use crate::program::GenericArg;

/// Type for StarkNet storage address, a value in the range [0, 2 ** 251 - 256).
#[derive(Default)]
pub struct StorageAddressType {}
impl NoGenericArgsGenericType for StorageAddressType {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("StorageAddress");

    fn specialize(&self) -> Self::Concrete {
        InfoOnlyConcreteType {
            info: TypeInfo {
                long_id: Self::concrete_type_long_id(&[]),
                storable: true,
                droppable: true,
                duplicatable: true,
                size: 1,
            },
        }
    }
}

/// LibFunc for creating a constant storage address.
#[derive(Default)]
pub struct StorageAddressConstLibFunc {}
impl NamedLibFunc for StorageAddressConstLibFunc {
    type Concrete = StorageAddressConstConcreteLibFunc;
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("storage_address_const");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        _args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        Ok(LibFuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(StorageAddressType::id(), &[])?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Const),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [GenericArg::Value(c)] => Ok(StorageAddressConstConcreteLibFunc {
                c: c.clone(),
                signature: <Self as NamedLibFunc>::specialize_signature(
                    self,
                    context.upcast(),
                    args,
                )?,
            }),
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}

pub struct StorageAddressConstConcreteLibFunc {
    pub c: BigInt,
    pub signature: LibFuncSignature,
}
impl SignatureBasedConcreteLibFunc for StorageAddressConstConcreteLibFunc {
    fn signature(&self) -> &LibFuncSignature {
        &self.signature
    }
}

/// LibFunc for a storage read system call.
#[derive(Default)]
pub struct StorageReadLibFunc {}
impl NoGenericArgsGenericLibFunc for StorageReadLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("storage_read_syscall");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let system_ty = context.get_concrete_type(SystemType::id(), &[])?;
        let addr_ty = context.get_concrete_type(StorageAddressType::id(), &[])?;
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        Ok(LibFuncSignature::new_non_branch_ex(
            vec![
                ParamSignature {
                    ty: system_ty.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
                ParamSignature::new(addr_ty),
            ],
            vec![
                OutputVarInfo {
                    ty: system_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                },
                OutputVarInfo {
                    ty: felt_ty,
                    ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                },
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }
}
