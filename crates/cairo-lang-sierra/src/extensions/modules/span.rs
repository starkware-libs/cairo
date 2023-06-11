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
        long_id: crate::program::ConcreteTypeLongId,
        TypeInfo { storable, duplicatable, droppable, size, .. }: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        if storable && size > 0 {
            Ok(TypeInfo { long_id, duplicatable, droppable, storable: true, size: 2 })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}
pub type SpanType = GenericTypeArgGenericTypeWrapper<SpanTypeWrapped>;
