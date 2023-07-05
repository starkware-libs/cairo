use std::sync::Arc;
use std::vec;

use cairo_lang_diagnostics::Maybe;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::items::structure::SemanticStructEx;
use cairo_lang_sierra::extensions::snapshot::snapshot_ty;
use cairo_lang_sierra::program::ConcreteTypeLongId;
use itertools::chain;

use crate::db::{SierraConcreteTypeLongIdCycleHandle, SierraGenGroup};
use crate::specialization_context::SierraSignatureSpecializationContext;

/// Interns and returns the Sierra concrete type id for a user defined struct or enum.
fn get_user_type_concrete_type_long_id<GenericArgTyIter: Iterator<Item = semantic::TypeId>>(
    db: &dyn SierraGenGroup,
    ty: semantic::ConcreteTypeId,
    generic_id: cairo_lang_sierra::ids::GenericTypeId,
    generic_arg_tys: GenericArgTyIter,
) -> Maybe<cairo_lang_sierra::program::ConcreteTypeLongId> {
    Ok(ConcreteTypeLongId {
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
    })
}

/// See [SierraGenGroup::get_concrete_type_id] for documentation.
pub fn get_concrete_type_id(
    db: &dyn SierraGenGroup,
    type_id: semantic::TypeId,
) -> Maybe<cairo_lang_sierra::ids::ConcreteTypeId> {
    if let semantic::TypeLongId::Concrete(semantic::ConcreteTypeId::Extern(extrn)) =
        db.lookup_intern_type(type_id)
    {
        if db.is_self_referential(type_id)?
            && matches!(
                extrn.extern_type_id(db.upcast()).name(db.upcast()).as_str(),
                "Box" | "Nullable"
            )
        {
            return Ok(
                db.intern_concrete_type(SierraConcreteTypeLongIdCycleHandle::CycleBreaker(type_id))
            );
        }
    }
    Ok(db.intern_concrete_type(SierraConcreteTypeLongIdCycleHandle::Regular(
        db.get_concrete_long_type_id(type_id)?,
    )))
}

/// See [SierraGenGroup::get_concrete_long_type_id] for documentation.
pub fn get_concrete_long_type_id(
    db: &dyn SierraGenGroup,
    type_id: semantic::TypeId,
) -> Maybe<Arc<cairo_lang_sierra::program::ConcreteTypeLongId>> {
    Ok(match db.lookup_intern_type(type_id) {
        semantic::TypeLongId::Concrete(ty) => {
            match ty {
                semantic::ConcreteTypeId::Struct(structure) => get_user_type_concrete_type_long_id(
                    db,
                    ty,
                    "Struct".into(),
                    db.concrete_struct_members(structure)?.into_iter().map(|(_, member)| member.ty),
                )?
                .into(),
                semantic::ConcreteTypeId::Enum(enm) => get_user_type_concrete_type_long_id(
                    db,
                    ty,
                    "Enum".into(),
                    db.concrete_enum_variants(enm)?.into_iter().map(|variant| variant.ty),
                )?
                .into(),
                semantic::ConcreteTypeId::Extern(extrn) => {
                    ConcreteTypeLongId {
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
        semantic::TypeLongId::Tuple(inner_types) => ConcreteTypeLongId {
            generic_id: "Struct".into(),
            generic_args: chain!(
                [cairo_lang_sierra::program::GenericArg::UserType("Tuple".into())],
                inner_types.into_iter().map(|ty| cairo_lang_sierra::program::GenericArg::Type(
                    db.get_concrete_type_id(ty).unwrap()
                ))
            )
            .collect(),
        }
        .into(),
        semantic::TypeLongId::Snapshot(ty) => {
            let inner_ty = db.get_concrete_type_id(ty).unwrap();
            let ty =
                snapshot_ty(&SierraSignatureSpecializationContext(db), inner_ty.clone()).unwrap();
            if ty == inner_ty {
                match db.lookup_intern_concrete_type(ty) {
                    SierraConcreteTypeLongIdCycleHandle::Regular(long_id) => long_id,
                    SierraConcreteTypeLongIdCycleHandle::CycleBreaker(ty) => {
                        return db.get_concrete_long_type_id(ty);
                    }
                }
            } else {
                ConcreteTypeLongId {
                    generic_id: "Snapshot".into(),
                    generic_args: vec![cairo_lang_sierra::program::GenericArg::Type(inner_ty)],
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
    match db.lookup_intern_type(type_id) {
        semantic::TypeLongId::Concrete(ty) => match ty {
            semantic::ConcreteTypeId::Struct(structure) => {
                for (_, member) in db.concrete_struct_members(structure)? {
                    db.is_self_referential(member.ty)?;
                }
            }
            semantic::ConcreteTypeId::Enum(enm) => {
                for variant in db.concrete_enum_variants(enm)? {
                    db.is_self_referential(variant.ty)?;
                }
            }
            semantic::ConcreteTypeId::Extern(_extrn) => {
                for arg in ty.generic_args(db.upcast()) {
                    if let semantic::GenericArgumentId::Type(ty) = arg {
                        db.is_self_referential(ty)?;
                    }
                }
            }
        },
        semantic::TypeLongId::Tuple(inner_types) => {
            for ty in inner_types {
                db.is_self_referential(ty)?;
            }
        }
        semantic::TypeLongId::Snapshot(ty) => {
            db.is_self_referential(ty)?;
        }
        semantic::TypeLongId::GenericParameter(_)
        | semantic::TypeLongId::Var(_)
        | semantic::TypeLongId::Missing(_) => {
            panic!(
                "Types should be fully resolved at this point. Got: `{}`.",
                type_id.format(db.upcast())
            )
        }
    }
    Ok(false)
}
