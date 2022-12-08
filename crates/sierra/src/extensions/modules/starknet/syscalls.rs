use crate::extensions::types::{InfoOnlyConcreteType, TypeInfo};
use crate::extensions::{NamedType, NoGenericArgsGenericType};
use crate::ids::GenericTypeId;

/// Type for StarkNet system call pointer.
/// Used to make system calls.
#[derive(Default)]
pub struct SyscallPtrType {}
impl NoGenericArgsGenericType for SyscallPtrType {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("SyscallPtr");

    fn specialize(&self) -> Self::Concrete {
        InfoOnlyConcreteType {
            info: TypeInfo {
                long_id: Self::concrete_type_long_id(&[]),
                storable: true,
                droppable: false,
                duplicatable: false,
                size: 1,
            },
        }
    }
}
