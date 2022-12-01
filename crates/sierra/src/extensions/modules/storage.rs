use super::syscalls::SyscallPtrType;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibFuncSignature, OutputVarInfo, SierraApChange,
    SignatureOnlyGenericLibFunc, SignatureSpecializationContext,
};
use crate::extensions::types::{InfoOnlyConcreteType, TypeInfo};
use crate::extensions::{
    NamedType, NoGenericArgsGenericType, OutputVarReferenceInfo, SpecializationError,
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

/// LibFunc for a storage read system call.
#[derive(Default)]
pub struct StorageRead {}
impl SignatureOnlyGenericLibFunc for StorageRead {
    const ID: GenericLibFuncId = GenericLibFuncId::new_inline("storage_read_syscall");

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        _args: &[GenericArg],
    ) -> Result<LibFuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(StorageAddressType::id(), &[])?;
        Ok(LibFuncSignature::new_non_branch(
            vec![context.get_concrete_type(SyscallPtrType::id(), &[])?],
            vec![OutputVarInfo {
                ty,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known(0),
        ))
    }
}
