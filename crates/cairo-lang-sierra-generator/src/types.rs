use std::sync::Arc;
use std::vec;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::items::structure::SemanticStructEx;
use cairo_lang_sierra::extensions::snapshot::snapshot_ty;
use cairo_lang_sierra::ids::UserTypeId;
use cairo_lang_sierra::program::{ConcreteTypeLongId, GenericArg as SierraGenericArg};
use cairo_lang_utils::try_extract_matches;
use itertools::chain;

use crate::db::{sierra_concrete_long_id, SierraGenGroup, SierraGeneratorTypeLongId};
use crate::specialization_context::SierraSignatureSpecializationContext;

/// See [SierraGenGroup::get_concrete_type_id] for documentation.
pub fn get_concrete_type_id(
    db: &dyn SierraGenGroup,
    type_id: semantic::TypeId,
) -> Maybe<cairo_lang_sierra::ids::ConcreteTypeId> {
    if matches!(
        db.lookup_intern_type(type_id),
        semantic::TypeLongId::Concrete(
            semantic::ConcreteTypeId::Enum(_) | semantic::ConcreteTypeId::Struct(_)
        )
    ) && db.is_self_referential(type_id)?
    {
        Ok(db.intern_concrete_type(SierraGeneratorTypeLongId::CycleBreaker(type_id)))
    } else {
        Ok(db.intern_concrete_type(SierraGeneratorTypeLongId::Regular(
            db.get_concrete_long_type_id(type_id)?,
        )))
    }
}

/// See [SierraGenGroup::get_concrete_long_type_id] for documentation.
pub fn get_concrete_long_type_id(
    db: &dyn SierraGenGroup,
    type_id: semantic::TypeId,
) -> Maybe<Arc<cairo_lang_sierra::program::ConcreteTypeLongId>> {
    let user_type_long_id = |generic_id: &str, user_type: UserTypeId| {
        let deps = db.type_dependencies(type_id)?;
        Ok(ConcreteTypeLongId {
            generic_id: generic_id.into(),
            generic_args: chain!(
                [Ok(SierraGenericArg::UserType(user_type))],
                deps.iter().map(|generic_arg_ty| db
                    .get_concrete_type_id(*generic_arg_ty)
                    .map(SierraGenericArg::Type))
            )
            .collect::<Maybe<_>>()?,
        })
    };
    Ok(match db.lookup_intern_type(type_id) {
        semantic::TypeLongId::Concrete(ty) => {
            match ty {
                semantic::ConcreteTypeId::Struct(_) => {
                    user_type_long_id("Struct", ty.format(db.upcast()).into())?.into()
                }
                semantic::ConcreteTypeId::Enum(_) => {
                    user_type_long_id("Enum", ty.format(db.upcast()).into())?.into()
                }
                semantic::ConcreteTypeId::Extern(extrn) => {
                    ConcreteTypeLongId {
                        // TODO(Gil): Implement name for semantic::ConcreteTypeId
                        generic_id: extrn.extern_type_id(db.upcast()).name(db.upcast()).into(),
                        generic_args: ty
                            .generic_args(db.upcast())
                            .into_iter()
                            .map(|arg| match arg {
                                semantic::GenericArgumentId::Type(ty) => {
                                    SierraGenericArg::Type(db.get_concrete_type_id(ty).unwrap())
                                }
                                semantic::GenericArgumentId::Literal(literal_id) => {
                                    SierraGenericArg::Value(
                                        db.lookup_intern_literal(literal_id).value,
                                    )
                                }
                                semantic::GenericArgumentId::Impl(_) => {
                                    panic!("Extern function with impl generics are not supported.")
                                }
                            })
                            .collect(),
                    }
                    .into()
                }
            }
        }
        semantic::TypeLongId::Tuple(_) => user_type_long_id("Struct", "Tuple".into())?.into(),
        semantic::TypeLongId::Snapshot(ty) => {
            let inner_ty = db.get_concrete_type_id(ty).unwrap();
            let ty =
                snapshot_ty(&SierraSignatureSpecializationContext(db), inner_ty.clone()).unwrap();
            if ty == inner_ty {
                return sierra_concrete_long_id(db, ty.clone());
            } else {
                ConcreteTypeLongId {
                    generic_id: "Snapshot".into(),
                    generic_args: vec![SierraGenericArg::Type(inner_ty)],
                }
                .into()
            }
        }
        semantic::TypeLongId::GenericParameter(_)
        | semantic::TypeLongId::Var(_)
        | semantic::TypeLongId::Missing(_) => {
            panic!(
                "Types should be fully resolved at this point. Got: `{}`.",
                type_id.format(db.upcast())
            )
        }
    })
}

/// See [SierraGenGroup::is_self_referential] for documentation.
pub fn is_self_referential_cycle(
    _db: &dyn SierraGenGroup,
    _cycle: &[String],
    _type_id: &semantic::TypeId,
) -> Maybe<bool> {
    Ok(true)
}

/// See [SierraGenGroup::is_self_referential] for documentation.
pub fn is_self_referential(db: &dyn SierraGenGroup, type_id: semantic::TypeId) -> Maybe<bool> {
    for ty in db.type_dependencies(type_id)?.iter() {
        db.is_self_referential(*ty)?;
    }
    Ok(false)
}

/// See [SierraGenGroup::type_dependencies] for documentation.
pub fn type_dependencies(
    db: &dyn SierraGenGroup,
    type_id: semantic::TypeId,
) -> Maybe<Arc<Vec<semantic::TypeId>>> {
    Ok(match db.lookup_intern_type(type_id) {
        semantic::TypeLongId::Concrete(ty) => match ty {
            semantic::ConcreteTypeId::Struct(structure) => db
                .concrete_struct_members(structure)?
                .into_iter()
                .map(|(_, member)| member.ty)
                .collect(),
            semantic::ConcreteTypeId::Enum(enm) => {
                db.concrete_enum_variants(enm)?.into_iter().map(|variant| variant.ty).collect()
            }
            semantic::ConcreteTypeId::Extern(_extrn) => ty
                .generic_args(db.upcast())
                .into_iter()
                .filter_map(|arg| try_extract_matches!(arg, semantic::GenericArgumentId::Type))
                .collect(),
        },
        semantic::TypeLongId::Tuple(inner_types) => inner_types,
        semantic::TypeLongId::Snapshot(ty) => vec![ty],
        semantic::TypeLongId::GenericParameter(_)
        | semantic::TypeLongId::Var(_)
        | semantic::TypeLongId::Missing(_) => {
            panic!(
                "Types should be fully resolved at this point. Got: `{}`.",
                type_id.format(db.upcast())
            )
        }
    }
    .into())
}
