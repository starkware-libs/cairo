use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::SpecializationError;
use crate::ids::GenericTypeId;

/// Uninitialized value of type T.
#[derive(Default)]
pub struct UninitializedTypeWrapped {}
impl GenericTypeArgGenericType for UninitializedTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("Uninitialized");

    fn calc_info(
        &self,
        _context: &dyn TypeSpecializationContext,
        long_id: crate::program::ConcreteTypeLongId,
        TypeInfo { storable, zero_sized, .. }: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        if storable {
            Ok(TypeInfo {
                long_id,
                storable: false,
                droppable: true,
                duplicatable: false,
                zero_sized,
            })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}
pub type UninitializedType = GenericTypeArgGenericTypeWrapper<UninitializedTypeWrapped>;
