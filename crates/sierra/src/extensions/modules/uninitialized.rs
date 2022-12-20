use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{args_as_single_type, ConcreteType, NamedType, SpecializationError};
use crate::ids::{ConcreteTypeId, GenericTypeId};
use crate::program::GenericArg;

/// Uninitialized value of type T.
#[derive(Default)]
pub struct UninitializedType {}
impl NamedType for UninitializedType {
    type Concrete = UninitializedConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("Uninitialized");

    fn specialize(
        &self,
        _context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Ok(UninitializedConcreteType {
            info: TypeInfo {
                long_id: Self::concrete_type_long_id(args),
                storable: false,
                droppable: true,
                duplicatable: false,
                size: 0,
            },
            ty: args_as_single_type(args)?,
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
