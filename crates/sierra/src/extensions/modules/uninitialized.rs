use super::as_single_type;
use crate::extensions::{ConcreteType, NamedType, SpecializationError};
use crate::ids::{ConcreteTypeId, GenericTypeId};
use crate::program::GenericArg;

/// Uninitialized value of type T.
#[derive(Default)]
pub struct UninitializedType {}
impl NamedType for UninitializedType {
    type Concrete = UninitializedConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("uninitialized");
    fn specialize(&self, args: &[GenericArg]) -> Result<Self::Concrete, SpecializationError> {
        Ok(UninitializedConcreteType { ty: as_single_type(args)? })
    }
}
pub struct UninitializedConcreteType {
    pub ty: ConcreteTypeId,
}
impl ConcreteType for UninitializedConcreteType {}
