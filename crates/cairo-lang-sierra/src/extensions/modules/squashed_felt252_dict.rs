use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::SpecializationError;
use crate::ids::GenericTypeId;

/// Type representing a static squashed dictionary from a felt252 to any type of size one.
#[derive(Default)]
pub struct SquashedFelt252DictTypeWrapped {}
impl GenericTypeArgGenericType for SquashedFelt252DictTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("SquashedFelt252Dict");

    fn calc_info(
        &self,
        _context: &dyn TypeSpecializationContext,
        long_id: crate::program::ConcreteTypeLongId,
        TypeInfo { zero_sized, storable, droppable, .. }: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        // Note: SquashedFelt252Dict is defined as non-duplicatable even if the inner type is
        // duplicatable to allow libfunc that adds entries to it (treat it similarly to an array).
        // TODO(Gil): the implementation support values of size 1. Remove when other sizes are
        // supported.
        if storable && !zero_sized {
            Ok(TypeInfo {
                long_id,
                storable: true,
                droppable,
                duplicatable: false,
                zero_sized: false,
            })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}
pub type SquashedFelt252DictType = GenericTypeArgGenericTypeWrapper<SquashedFelt252DictTypeWrapped>;
