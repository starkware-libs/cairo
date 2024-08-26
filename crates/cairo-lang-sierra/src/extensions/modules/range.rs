use super::bounded_int::BoundedIntType;
use super::int::signed::{Sint16Type, Sint32Type, Sint64Type, Sint8Type};
use super::int::signed128::Sint128Type;
use super::int::unsigned::{Uint16Type, Uint32Type, Uint64Type, Uint8Type};
use super::int::unsigned128::Uint128Type;
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::{
    GenericTypeArgGenericType, GenericTypeArgGenericTypeWrapper, TypeInfo,
};
use crate::extensions::{NamedType, SpecializationError};
use crate::ids::GenericTypeId;
use crate::program::GenericArg;

fn check_inner_type(ty_info: &TypeInfo) -> Result<(), SpecializationError> {
    // Note: the implementation assumes the following types are of size 1.
    match (&ty_info.long_id.generic_id, &ty_info.long_id.generic_args[..]) {
        (id, []) if *id == Uint8Type::id() => (),
        (id, []) if *id == Uint16Type::id() => (),
        (id, []) if *id == Uint32Type::id() => (),
        (id, []) if *id == Uint64Type::id() => (),
        (id, []) if *id == Uint128Type::id() => (),
        (id, []) if *id == Sint8Type::id() => (),
        (id, []) if *id == Sint16Type::id() => (),
        (id, []) if *id == Sint32Type::id() => (),
        (id, []) if *id == Sint64Type::id() => (),
        (id, []) if *id == Sint128Type::id() => (),
        (id, [GenericArg::Value(_), GenericArg::Value(_)]) if *id == BoundedIntType::id() => (),
        _ => return Err(SpecializationError::UnsupportedGenericArg),
    };
    Ok(())
}

/// Type for `IntRange(x, y)` where `x <= y`.
#[derive(Default)]
pub struct IntRangeTypeWrapped {}
impl GenericTypeArgGenericType for IntRangeTypeWrapped {
    const ID: GenericTypeId = GenericTypeId::new_inline("IntRange");

    fn calc_info(
        &self,
        _context: &dyn TypeSpecializationContext,
        long_id: crate::program::ConcreteTypeLongId,
        wrapped_info: TypeInfo,
    ) -> Result<TypeInfo, SpecializationError> {
        check_inner_type(&wrapped_info)?;

        // The following assert is a sanity check. It should follow from the fact that
        // `check_inner_type` passed.
        assert!(
            wrapped_info.storable
                && wrapped_info.duplicatable
                && wrapped_info.droppable
                && !wrapped_info.zero_sized
        );
        Ok(TypeInfo {
            long_id,
            duplicatable: true,
            droppable: true,
            storable: true,
            zero_sized: false,
        })
    }
}
pub type IntRangeType = GenericTypeArgGenericTypeWrapper<IntRangeTypeWrapped>;
