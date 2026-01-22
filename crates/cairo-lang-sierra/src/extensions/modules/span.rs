use crate::extensions::SpecializationError;
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
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
        wrapped_info: &TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        if wrapped_info.storable && !wrapped_info.zero_sized {
            Ok(TypeInfo {
                long_id,
                duplicatable: wrapped_info.duplicatable,
                droppable: wrapped_info.droppable,
                storable: true,
                zero_sized: false,
            })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}
pub type SpanType = GenericTypeArgGenericTypeWrapper<SpanTypeWrapped>;
