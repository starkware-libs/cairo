use super::as_single_type;
use crate::extensions::types::{TypeInfo, TypeSpecializationContext};
use crate::extensions::{ConcreteType, NamedType, SpecializationError};
use crate::ids::{ConcreteTypeId, GenericTypeId};
use crate::program::GenericArg;

/// Uninitialized value of type T.
#[derive(Default)]
pub struct UninitializedType {}
impl NamedType for UninitializedType {
    type Concrete = UninitializedConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("uninitialized");

    fn specialize(
        &self,
        _context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(UninitializedConcreteType {
            info: TypeInfo { storable: false, droppable: true, duplicatable: false },
            ty: as_single_type(args)?,
        })
    }
}
pub struct UninitializedConcreteType {
    pub info: TypeInfo,
    pub ty: ConcreteTypeId,
}
impl ConcreteType for UninitializedConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}
