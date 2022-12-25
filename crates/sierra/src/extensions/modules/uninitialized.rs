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
        long_id: crate::program::ConcreteTypeLongId,
        wrapped_info: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        if !wrapped_info.storable {
            Err(SpecializationError::UnsupportedGenericArg)
        } else {
            Ok(TypeInfo { long_id, storable: false, droppable: true, duplicatable: false, size: 0 })
        }
    }
}
pub type UninitializedType = GenericTypeArgGenericTypeWrapper<UninitializedTypeWrapped>;
