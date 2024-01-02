use super::utils::Range;
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{ConcreteType, NamedType, SpecializationError};
use crate::ids::GenericTypeId;
use crate::program::GenericArg;

/// Type for Felt252Bounded.
/// The native type of the Cairo architecture.
#[derive(Default)]
pub struct Felt252BoundedType {}
impl NamedType for Felt252BoundedType {
    type Concrete = Felt252BoundedConcreteType;

    // TODO: rename lower_bound and upper_bound to min and max. Our convention is that "upper bound"
    //   is exclusive and "max" is inclusive (lower_bound and min are both inclusive).
    // TODO: Consider changing to bounded int, and then we need to restrict the size of the range
    //   to be lower than field prime.
    const ID: GenericTypeId = GenericTypeId::new_inline("Felt252Bounded");
    fn specialize(
        &self,
        _context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let (lower_bound, upper_bound) = match args {
            [GenericArg::Value(lower_bound), GenericArg::Value(upper_bound)] => {
                (lower_bound.clone(), upper_bound.clone())
            }
            [_, _] => return Err(SpecializationError::UnsupportedGenericArg),
            _ => return Err(SpecializationError::WrongNumberOfGenericArgs),
        };

        // TODO: What happens if the difference is >= field prime?
        if lower_bound > upper_bound {
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

        Ok(Self::Concrete {
            info: ty_info,
            range: Range { lower: lower_bound, upper: upper_bound + 1 },
        })
    }
}

pub struct Felt252BoundedConcreteType {
    pub info: TypeInfo,
    /// The range bounds for a value of this type.
    pub range: Range,
}
impl ConcreteType for Felt252BoundedConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}
