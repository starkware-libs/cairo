use itertools::chain;
use semantic::items::enm::SemanticEnumEx;
use sierra::program::ConcreteTypeLongId;

use crate::db::SierraGenGroup;

/// See [SierraGenGroup::get_concrete_type_id] for documentation.
pub fn get_concrete_type_id(
    db: &dyn SierraGenGroup,
    type_id: semantic::TypeId,
) -> Option<sierra::ids::ConcreteTypeId> {
    match db.lookup_intern_type(type_id) {
        semantic::TypeLongId::Concrete(ty) => {
            match ty {
                semantic::ConcreteTypeId::Struct(_) => todo!(),
                semantic::ConcreteTypeId::Enum(enm) => {
                    let generic_args: Option<_> = chain!(
                        [Some(sierra::program::GenericArg::UserType(
                            type_id.format(db.upcast()).into()
                        ))],
                        db.concrete_enum_variants(enm)?.into_iter().map(|concrete_variant| {
                            Some(sierra::program::GenericArg::Type(
                                db.get_concrete_type_id(concrete_variant.ty)?,
                            ))
                        })
                    )
                    .collect();
                    Some(db.intern_concrete_type(ConcreteTypeLongId {
                        generic_id: "Enum".into(),
                        generic_args: generic_args?,
                    }))
                }
                semantic::ConcreteTypeId::Extern(extrn) => {
                    Some(
                        db.intern_concrete_type(ConcreteTypeLongId {
                            generic_id: sierra::ids::GenericTypeId::from_string(
                                // TODO(Gil): Implement name for semantic::ConcreteTypeId
                                extrn.extern_type_id(db.upcast()).name(db.upcast()),
                            ),
                            generic_args: ty
                                .generic_args(db.upcast())
                                .into_iter()
                                .map(|arg| match arg {
                                    semantic::GenericArgumentId::Type(ty) => {
                                        sierra::program::GenericArg::Type(
                                            db.get_concrete_type_id(ty).unwrap(),
                                        )
                                    }
                                })
                                .collect(),
                        }),
                    )
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
