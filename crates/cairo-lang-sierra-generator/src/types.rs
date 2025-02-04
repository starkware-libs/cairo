use std::sync::Arc;
use std::vec;

use cairo_lang_defs::ids::NamedLanguageElementId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_lowering::ids::SemanticFunctionIdEx;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_sierra::extensions::snapshot::snapshot_ty;
use cairo_lang_sierra::ids::UserTypeId;
use cairo_lang_sierra::program::{ConcreteTypeLongId, GenericArg as SierraGenericArg};
use cairo_lang_utils::{Intern, LookupIntern, try_extract_matches};
use itertools::chain;
use num_traits::ToPrimitive;
use semantic::items::imp::ImplLookupContext;

use crate::db::{SierraGenGroup, SierraGeneratorTypeLongId, sierra_concrete_long_id};
use crate::specialization_context::SierraSignatureSpecializationContext;

/// See [SierraGenGroup::get_concrete_type_id] for documentation.
pub fn get_concrete_type_id(
    db: &dyn SierraGenGroup,
    type_id: semantic::TypeId,
) -> Maybe<cairo_lang_sierra::ids::ConcreteTypeId> {
    match type_id.lookup_intern(db) {
        semantic::TypeLongId::Snapshot(inner_ty)
            if db.type_info(ImplLookupContext::default(), inner_ty)?.copyable.is_ok() =>
        {
            db.get_concrete_type_id(inner_ty)
        }
        semantic::TypeLongId::Concrete(
            semantic::ConcreteTypeId::Enum(_) | semantic::ConcreteTypeId::Struct(_),
        ) if db.is_self_referential(type_id)? => {
            Ok(SierraGeneratorTypeLongId::CycleBreaker(type_id).intern(db))
        }
        _ => Ok(if type_id.is_phantom(db.upcast()) {
            SierraGeneratorTypeLongId::Phantom(type_id)
        } else {
            SierraGeneratorTypeLongId::Regular(db.get_concrete_long_type_id(type_id)?)
        }
        .intern(db)),
    }
}

/// See [SierraGenGroup::get_index_enum_type_id] for documentation.
pub fn get_index_enum_type_id(
    db: &dyn SierraGenGroup,
    index_count: usize,
) -> Maybe<cairo_lang_sierra::ids::ConcreteTypeId> {
    let unit_ty_arg = db
        .get_concrete_type_id(semantic::TypeLongId::Tuple(vec![]).intern(db))
        .map(SierraGenericArg::Type)?;
    let generic_args = chain!(
        [SierraGenericArg::UserType(format!("index_enum_type<{}>", index_count).into())],
        itertools::repeat_n(unit_ty_arg, index_count)
    )
    .collect();
    let x = SierraGeneratorTypeLongId::Regular(
        ConcreteTypeLongId { generic_id: "Enum".into(), generic_args }.into(),
    );

    Ok(x.intern(db))
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
    Ok(match type_id.lookup_intern(db) {
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
                                semantic::GenericArgumentId::Constant(value_id) => {
                                    let value = value_id
                                        .lookup_intern(db)
                                        .into_int()
                                        .expect("Expected ConstValue::Int for size");

                                    SierraGenericArg::Value(value)
                                }
                                semantic::GenericArgumentId::Impl(_) => {
                                    panic!("Extern function with impl generics are not supported.")
                                }
                                semantic::GenericArgumentId::NegImpl => panic!(
                                    "Extern function with neg impl generics are not supported."
                                ),
                            })
                            .collect(),
                    }
                    .into()
                }
            }
        }
        semantic::TypeLongId::Tuple(_) | semantic::TypeLongId::FixedSizeArray { .. } => {
            user_type_long_id("Struct", "Tuple".into())?.into()
        }
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
        semantic::TypeLongId::Coupon(function_id) => ConcreteTypeLongId {
            generic_id: "Coupon".into(),
            generic_args: vec![SierraGenericArg::UserFunc(
                function_id.lowered(db.upcast()).intern(db),
            )],
        }
        .into(),
        semantic::TypeLongId::GenericParameter(_)
        | semantic::TypeLongId::Var(_)
        | semantic::TypeLongId::ImplType(_)
        | semantic::TypeLongId::Missing(_) => {
            panic!(
                "Types should be fully resolved at this point. Got: `{}`.",
                type_id.format(db.upcast())
            )
        }
        semantic::TypeLongId::Closure(_) => {
            user_type_long_id("Struct", (type_id.format(db.upcast())).into())?.into()
        }
    })
}

/// See [SierraGenGroup::is_self_referential] for documentation.
pub fn is_self_referential(db: &dyn SierraGenGroup, type_id: semantic::TypeId) -> Maybe<bool> {
    db.has_in_deps(type_id, type_id)
}

/// See [SierraGenGroup::type_dependencies] for documentation.
pub fn type_dependencies(
    db: &dyn SierraGenGroup,
    type_id: semantic::TypeId,
) -> Maybe<Arc<[semantic::TypeId]>> {
    Ok(match type_id.lookup_intern(db) {
        semantic::TypeLongId::Concrete(ty) => match ty {
            semantic::ConcreteTypeId::Struct(structure) => {
                db.concrete_struct_members(structure)?.iter().map(|(_, member)| member.ty).collect()
            }
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
        semantic::TypeLongId::Coupon(_) => vec![],
        semantic::TypeLongId::Closure(closure_ty) => closure_ty.captured_types,
        semantic::TypeLongId::FixedSizeArray { type_id, size } => {
            let size = size
                .lookup_intern(db)
                .into_int()
                .expect("Expected ConstValue::Int for size")
                .to_usize()
                .unwrap();
            [type_id].repeat(size)
        }
        semantic::TypeLongId::GenericParameter(_)
        | semantic::TypeLongId::Var(_)
        | semantic::TypeLongId::ImplType(_)
        | semantic::TypeLongId::Missing(_) => {
            panic!(
                "Types should be fully resolved at this point. Got: `{}`.",
                type_id.format(db.upcast())
            )
        }
    }
    .into())
}

/// See [SierraGenGroup::has_in_deps] for documentation.
pub fn has_in_deps(
    db: &dyn SierraGenGroup,
    type_id: semantic::TypeId,
    needle: semantic::TypeId,
) -> Maybe<bool> {
    let deps = type_dependencies(db, type_id)?;
    if deps.contains(&needle) {
        return Ok(true);
    }
    for dep in deps.iter() {
        if db.has_in_deps(*dep, needle)? {
            return Ok(true);
        }
    }
    Ok(false)
}

/// See [SierraGenGroup::has_in_deps] for documentation.
pub fn has_in_deps_cycle(
    _db: &dyn SierraGenGroup,
    _cycle: &salsa::Cycle,
    _type_id: &semantic::TypeId,
    _needle: &semantic::TypeId,
) -> Maybe<bool> {
    Ok(false)
}
