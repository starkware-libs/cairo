use crate::extensions::types::{InfoOnlyConcreteType, NamedType, TypeInfo};
use crate::extensions::NoGenericArgsGenericType;
use crate::ids::GenericTypeId;

/// Type for Range Check builtin.
#[derive(Default)]
pub struct RangeCheckType {}
impl NoGenericArgsGenericType for RangeCheckType {
    type Concrete = InfoOnlyConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("RangeCheck");

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
