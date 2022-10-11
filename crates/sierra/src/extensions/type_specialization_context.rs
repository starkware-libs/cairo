use super::types::TypeInfo;
use super::SpecializationError;
use crate::ids::ConcreteTypeId;

/// Trait for the specialization of types.
pub trait TypeSpecializationContext {
    /// Returns the type information for the type with the given id.
    fn try_get_type_info(&self, id: ConcreteTypeId) -> Option<TypeInfo>;

    /// Wraps [Self::try_get_type_info] with a result object.
    fn get_type_info(&self, id: ConcreteTypeId) -> Result<TypeInfo, SpecializationError> {
        self.try_get_type_info(id.clone()).ok_or(SpecializationError::MissingTypeInfo(id))
    }
}
