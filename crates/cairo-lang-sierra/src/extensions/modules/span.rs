use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::SpecializationError;
use crate::ids::GenericTypeId;

/// Type representing a span.
#[derive(Default)]
pub struct SpanTypeWrapped {}
impl GenericTypeArgGenericType for SpanTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("Span");

    fn calc_info(
        &self,
        _context: &dyn TypeSpecializationContext,
        long_id: crate::program::ConcreteTypeLongId,
        TypeInfo { storable, duplicatable, droppable, zero_sized, .. }: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        if storable && !zero_sized {
            Ok(TypeInfo { long_id, duplicatable, droppable, storable: true, zero_sized: false })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}
pub type SpanType = GenericTypeArgGenericTypeWrapper<SpanTypeWrapped>;
