use utils::try_extract_matches;

use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{ConcreteType, NamedType, SpecializationError};
use crate::ids::GenericTypeId;
use crate::program::{ConcreteTypeLongId, GenericArg};

#[derive(Default)]
pub struct UnitType {}
impl NamedType for UnitType {
    type Concrete = UnitConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("Unit");

    fn specialize(
        &self,
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Self::Concrete::new(context, args)
    }
}

pub struct UnitConcreteType {
    pub info: TypeInfo,
}
impl UnitConcreteType {
    fn new(
        context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self, SpecializationError> {
        let arg = match args {
            [ar] => Ok(ar),
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
        .unwrap();
        let ty = try_extract_matches!(arg, GenericArg::Type)
            .ok_or(SpecializationError::UnsupportedGenericArg)?
            .clone();
        let info = context.get_type_info(ty.clone())?;
        Ok(UnitConcreteType {
            info: TypeInfo {
                long_id: ConcreteTypeLongId {
                    generic_id: "Unit".into(),
                    generic_args: args.to_vec(),
                },
                duplicatable: info.duplicatable,
                droppable: info.droppable,
                storable: info.storable,
                size: info.size,
            },
        })
    }
}
impl ConcreteType for UnitConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}
