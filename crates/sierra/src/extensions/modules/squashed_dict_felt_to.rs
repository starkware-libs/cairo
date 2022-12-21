use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{args_as_single_type, ConcreteType, NamedType, SpecializationError};
use crate::ids::{ConcreteTypeId, GenericTypeId};
use crate::program::GenericArg;

/// Type representing a static squashed dictionary from a felt to any type of size one.
#[derive(Default)]
pub struct SquashedDictFeltToType {}
impl NamedType for SquashedDictFeltToType {
    type Concrete = SquashedDictFeltToConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("SquashedDictFeltTo");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let info = context.get_type_info(ty.clone())?;
        // TODO(Gil): the implementation support values of size 1. Remove when other sizes are
        // supported.
        if info.storable && info.size == 1 {
            Ok(SquashedDictFeltToConcreteType {
                info: TypeInfo {
                    long_id: Self::concrete_type_long_id(args),
                    duplicatable: false,
                    droppable: info.droppable,
                    storable: true,
                    size: 2,
                },
                ty,
            })
        } else {
            Err(SpecializationError::UnsupportedGenericArg)
        }
    }
}

pub struct SquashedDictFeltToConcreteType {
    pub info: TypeInfo,
    pub ty: ConcreteTypeId,
}

impl ConcreteType for SquashedDictFeltToConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}
