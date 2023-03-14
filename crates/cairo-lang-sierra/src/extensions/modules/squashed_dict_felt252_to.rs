use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::SpecializationError;
use crate::ids::GenericTypeId;

/// Type representing a static squashed dictionary from a felt252 to any type of size one.
#[derive(Default)]
pub struct SquashedDictFelt252ToTypeWrapped {}
impl GenericTypeArgGenericType for SquashedDictFelt252ToTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("SquashedDictFelt252To");

    fn calc_info(
        &self,
        long_id: crate::program::ConcreteTypeLongId,
        TypeInfo { size, storable, droppable, .. }: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        // TODO(Gil): the implementation support values of size 1. Remove when other sizes are
        // supported.
        if storable && size == 1 {
            Ok(TypeInfo { long_id, duplicatable: false, droppable, storable: true, size: 2 })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}
pub type SquashedDictFelt252ToType =
    GenericTypeArgGenericTypeWrapper<SquashedDictFelt252ToTypeWrapped>;
