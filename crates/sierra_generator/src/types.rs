use itertools::chain;
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
            for arg in ty.generic_args(db.upcast()) {
                match arg {
                    semantic::GenericArgumentId::Type(ty) => {
                        generic_args
                            .push(sierra::program::GenericArg::Type(db.get_concrete_type_id(ty)?));
                    }
                }
            }
            match ty.generic_type(db.upcast()) {
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
        semantic::TypeLongId::Tuple(inner_types) => Some(
            db.intern_concrete_type(ConcreteTypeLongId {
                generic_id: "Struct".into(),
                generic_args: chain!(
                    [sierra::program::GenericArg::UserType("Tuple".into())],
                    inner_types.into_iter().map(|ty| sierra::program::GenericArg::Type(
                        db.get_concrete_type_id(ty).unwrap()
                    ))
                )
                .collect(),
            }),
        ),
        semantic::TypeLongId::GenericParameter(_) => todo!("Add support for generic parameters."),
        semantic::TypeLongId::Missing | semantic::TypeLongId::Never => None,
    }
}
