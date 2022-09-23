use defs::ids::LanguageElementId;
use sierra::program::ConcreteTypeLongId;

use crate::db::SierraGenGroup;

/// See [SierraGenGroup::get_concrete_type_id] for documentation.
pub fn get_concrete_type_id(
    db: &dyn SierraGenGroup,
    type_id: semantic::TypeId,
) -> Option<sierra::ids::ConcreteTypeId> {
    match db.lookup_intern_type(type_id) {
        semantic::TypeLongId::Concrete(ty) => {
            let mut generic_args = vec![];
            for arg in &ty.generic_args {
                match arg {
                    semantic::GenericArgumentId::Type(ty) => {
                        generic_args
                            .push(sierra::program::GenericArg::Type(db.get_concrete_type_id(*ty)?));
                    }
                }
            }
            match ty.generic_type {
                defs::ids::GenericTypeId::Struct(_) => {
                    todo!("Add support for struct types when they are supported in Sierra.")
                }
                defs::ids::GenericTypeId::Enum(_) => {
                    todo!("Add support for enum types when they are supported in Sierra.")
                }
                defs::ids::GenericTypeId::Extern(extrn) => {
                    Some(db.intern_concrete_type(ConcreteTypeLongId {
                        generic_id: sierra::ids::GenericTypeId::from_string(
                            extrn.name(db.upcast()),
                        ),
                        generic_args,
                    }))
                }
            }
        }
        semantic::TypeLongId::Tuple(_) => {
            todo!("Add support for tuple types when they are supported in Sierra.")
        }
        semantic::TypeLongId::GenericParameter(_) => todo!("Add support for generic parameters."),
        semantic::TypeLongId::Missing => None,
    }
}
