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
            let mut generic_args_iter = 
            ty.generic_args(db.upcast()).into_iter().map(|arg|
                match arg {
                    semantic::GenericArgumentId::Type(ty) => {
                        sierra::program::GenericArg::Type(db.get_concrete_type_id(ty).unwrap())
                    },
                }
            );
            match ty.generic_type(db.upcast()) {
                defs::ids::GenericTypeId::Struct(_) => {
                    todo!("Add support for struct types when they are supported in Sierra.")
                }
                defs::ids::GenericTypeId::Enum(enm) => {
                    // TODO(Gil): Consider interning the UserType.
                    let variant_args = chain!(
                        [sierra::program::GenericArg::UserType(
                            enm.name(db.upcast()).to_string().into()
                        )],
                        db.enum_variants(enm)?.into_iter().map(|(_, varinat_id)| {
                            db.variant_semantic(enm, varinat_id)
                                .map(|variant| {
                                    match db.lookup_intern_type(variant.ty) {
                                        semantic::TypeLongId::Concrete(_) | semantic::TypeLongId::Tuple(_) => 
                                        sierra::program::GenericArg::Type(
                                            db.get_concrete_type_id(variant.ty).unwrap(),
                                        ),
                                        semantic::TypeLongId::GenericParameter(_) => {
                                            generic_args_iter.next().unwrap()
                                        },
                                        semantic::TypeLongId::Never | semantic::TypeLongId::Missing => 
                                        {
                                            panic!("Unknown type as a generic type of an Enum.")
                                        },
                                    }
                                })
                                .unwrap()
                        })
                    )
                    .collect();
                    Some(db.intern_concrete_type(ConcreteTypeLongId {
                        generic_id: "Enum".into(),
                        generic_args: variant_args,
                    }))
                }
                defs::ids::GenericTypeId::Extern(extrn) => {
                    Some(db.intern_concrete_type(ConcreteTypeLongId {
                        generic_id: sierra::ids::GenericTypeId::from_string(
                            extrn.name(db.upcast()),
                        ),
                        generic_args: generic_args_iter.collect(),
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
        semantic::TypeLongId::GenericParameter(_) 
        | semantic::TypeLongId::Missing 
        | semantic::TypeLongId::Never => None,
    }
}
