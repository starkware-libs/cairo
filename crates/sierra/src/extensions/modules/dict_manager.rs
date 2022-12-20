use crate::extensions::types::{InfoOnlyConcreteType, NamedType, TypeInfo};
use crate::extensions::NoGenericArgsGenericType;
use crate::ids::GenericTypeId;

/// Type for the Dict Manager builtin.
/// This type should be initialized and destructed by the OS.
/// It is assumed to be a pointer to a segment containing the following struct:
/// dict_infos segment start
/// dict_infos segment end
/// Number of destructed dictionaries
/// On each dict_new/dict_finalize the struct is appended to the segment and the returned pointer is
/// incremented accrodingly.
#[derive(Default)]
pub struct DictManagerType {}
impl NoGenericArgsGenericType for DictManagerType {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("DictManager");

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
