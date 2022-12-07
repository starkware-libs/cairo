use super::felt::FeltType;
use super::syscalls::SyscallPtrType;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibFuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureSpecializationContext,
};
use crate::extensions::types::{InfoOnlyConcreteType, TypeInfo};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibFunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::{GenericLibFuncId, GenericTypeId};

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

/// LibFunc for a storage read system call.
#[derive(Default)]
pub struct StorageReadLibFunc {}
impl NoGenericArgsGenericLibFunc for StorageReadLibFunc {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("storage_read_syscall");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibFuncSignature, SpecializationError> {
        let syscall_ptr_ty = context.get_concrete_type(SyscallPtrType::id(), &[])?;
        let addr_ty = context.get_concrete_type(StorageAddressType::id(), &[])?;
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        Ok(LibFuncSignature::new_non_branch_ex(
            vec![
                ParamSignature {
                    ty: syscall_ptr_ty.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
                ParamSignature::new(addr_ty),
            ],
            vec![
                OutputVarInfo {
                    ty: syscall_ptr_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                        param_idx: 0,
                    }),
                },
                OutputVarInfo {
                    ty: felt_ty,
                    ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                },
            ],
            SierraApChange::Known(0),
        ))
    }
}
