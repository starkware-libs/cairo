use itertools::chain;
use semantic::items::enm::SemanticEnumEx;
use semantic::items::strct::SemanticStructEx;
use sierra::program::ConcreteTypeLongId;

use crate::db::SierraGenGroup;

/// Interns and returns the Sierra concrete type id for a user defined struct or enum.
fn get_user_type_concrete_type_id<GenericArgTyIter: Iterator<Item = semantic::TypeId>>(
    db: &dyn SierraGenGroup,
    ty: semantic::ConcreteTypeId,
    generic_id: sierra::ids::GenericTypeId,
    generic_arg_tys: GenericArgTyIter,
) -> Option<sierra::ids::ConcreteTypeId> {
    Some(
        db.intern_concrete_type(ConcreteTypeLongId {
            generic_id,
            generic_args: chain!(
                [Some(sierra::program::GenericArg::UserType(ty.format(db.upcast()).into()))],
                generic_arg_tys.map(|generic_arg_ty| {
                    Some(sierra::program::GenericArg::Type(
                        db.get_concrete_type_id(generic_arg_ty)?,
                    ))
                })
            )
            .collect::<Option<_>>()?,
        }),
    )
}

/// See [SierraGenGroup::get_concrete_type_id] for documentation.
pub fn get_concrete_type_id(
    db: &dyn SierraGenGroup,
    type_id: semantic::TypeId,
) -> Option<sierra::ids::ConcreteTypeId> {
    match db.lookup_intern_type(type_id) {
        semantic::TypeLongId::Concrete(ty) => {
            match ty {
                semantic::ConcreteTypeId::Struct(strct) => get_user_type_concrete_type_id(
                    db,
                    ty,
                    "Struct".into(),
                    db.concrete_struct_members(strct)?.into_iter().map(|(_, member)| member.ty),
                ),
                semantic::ConcreteTypeId::Enum(enm) => get_user_type_concrete_type_id(
                    db,
                    ty,
                    "Enum".into(),
                    db.concrete_enum_variants(enm)?.into_iter().map(|variant| variant.ty),
                ),
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
        semantic::TypeLongId::GenericParameter(_) | semantic::TypeLongId::Missing => None,
    }
}
