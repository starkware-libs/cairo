use cairo_lang_diagnostics::{skip_diagnostic, Maybe};
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::items::strct::SemanticStructEx;
use cairo_lang_sierra::program::ConcreteTypeLongId;
use itertools::chain;

use crate::db::SierraGenGroup;

/// Interns and returns the Sierra concrete type id for a user defined struct or enum.
fn get_user_type_concrete_type_id<GenericArgTyIter: Iterator<Item = semantic::TypeId>>(
    db: &dyn SierraGenGroup,
    ty: semantic::ConcreteTypeId,
    generic_id: cairo_lang_sierra::ids::GenericTypeId,
    generic_arg_tys: GenericArgTyIter,
) -> Maybe<cairo_lang_sierra::ids::ConcreteTypeId> {
    Ok(db.intern_concrete_type(ConcreteTypeLongId {
        generic_id,
        generic_args: chain!(
            [Ok(cairo_lang_sierra::program::GenericArg::UserType(ty.format(db.upcast()).into()))],
            generic_arg_tys.map(|generic_arg_ty| {
                Ok(cairo_lang_sierra::program::GenericArg::Type(
                    db.get_concrete_type_id(generic_arg_ty)?,
                ))
            })
        )
        .collect::<Maybe<_>>()?,
    }))
}

/// See [SierraGenGroup::get_concrete_type_id] for documentation.
pub fn get_concrete_type_id(
    db: &dyn SierraGenGroup,
    type_id: semantic::TypeId,
) -> Maybe<cairo_lang_sierra::ids::ConcreteTypeId> {
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
                    Ok(db.intern_concrete_type(ConcreteTypeLongId {
                        generic_id: cairo_lang_sierra::ids::GenericTypeId::from_string(
                            // TODO(Gil): Implement name for semantic::ConcreteTypeId
                            extrn.extern_type_id(db.upcast()).name(db.upcast()),
                        ),
                        generic_args: ty
                            .generic_args(db.upcast())
                            .into_iter()
                            .map(|arg| match arg {
                                semantic::GenericArgumentId::Type(ty) => {
                                    cairo_lang_sierra::program::GenericArg::Type(
                                        db.get_concrete_type_id(ty).unwrap(),
                                    )
                                }
                                semantic::GenericArgumentId::Literal(literal_id) => {
                                    cairo_lang_sierra::program::GenericArg::Value(
                                        db.lookup_intern_literal(literal_id).value,
                                    )
                                }
                            })
                            .collect(),
                    }))
                }
            }
        }
        semantic::TypeLongId::Tuple(inner_types) => {
            Ok(db.intern_concrete_type(ConcreteTypeLongId {
                generic_id: "Struct".into(),
                generic_args: chain!(
                    [cairo_lang_sierra::program::GenericArg::UserType("Tuple".into())],
                    inner_types.into_iter().map(|ty| cairo_lang_sierra::program::GenericArg::Type(
                        db.get_concrete_type_id(ty).unwrap()
                    ))
                )
                .collect(),
            }))
        }
        semantic::TypeLongId::GenericParameter(_) => Err(skip_diagnostic()),
        semantic::TypeLongId::Var(_) => panic!("Types should be fully resolved at this point."),
        semantic::TypeLongId::Missing(diag_added) => Err(diag_added),
    }
}
