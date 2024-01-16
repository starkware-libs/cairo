use cairo_felt::Felt252;
use num_bigint::BigInt;

use super::utils::Range;
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{ConcreteType, NamedType, SpecializationError};
use crate::ids::GenericTypeId;
use crate::program::GenericArg;

/// Type for BoundedInt.
/// The native type of the Cairo architecture.
#[derive(Default)]
pub struct BoundedIntType {}
impl NamedType for BoundedIntType {
    type Concrete = BoundedIntConcreteType;

    const ID: GenericTypeId = GenericTypeId::new_inline("BoundedInt");
    fn specialize(
        &self,
        _context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let (min, max) = match args {
            [GenericArg::Value(min), GenericArg::Value(max)] => (min.clone(), max.clone()),
            [_, _] => return Err(SpecializationError::UnsupportedGenericArg),
            _ => return Err(SpecializationError::WrongNumberOfGenericArgs),
        };

        let prime: BigInt = Felt252::prime().into();
        if !(-&prime < min && min <= max && max < prime && &max - &min < prime) {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        let long_id = Self::concrete_type_long_id(args);
        let ty_info = TypeInfo {
            long_id,
            zero_sized: false,
            storable: true,
            droppable: true,
            duplicatable: true,
        };

        Ok(Self::Concrete { info: ty_info, range: Range::closed(min, max) })
    }
}

pub struct BoundedIntConcreteType {
    pub info: TypeInfo,
    /// The range bounds for a value of this type.
    pub range: Range,
}
impl ConcreteType for BoundedIntConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}
