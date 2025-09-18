use std::sync::Arc;
use std::vec;

use cairo_lang_defs::ids::NamedLanguageElementId;
use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::ids::Tracked;
use cairo_lang_lowering::ids::SemanticFunctionIdEx;
use cairo_lang_semantic as semantic;
use cairo_lang_semantic::items::enm::SemanticEnumEx;
use cairo_lang_semantic::items::structure::StructSemantic;
use cairo_lang_semantic::types::TypesSemantic;
use cairo_lang_sierra::extensions::snapshot::snapshot_ty;
use cairo_lang_sierra::ids::UserTypeId;
use cairo_lang_sierra::program::{ConcreteTypeLongId, GenericArg as SierraGenericArg};
use cairo_lang_utils::{Intern, try_extract_matches};
use itertools::chain;
use num_traits::ToPrimitive;
use salsa::Database;

use crate::db::{SierraGenGroup, SierraGeneratorTypeLongId, sierra_concrete_long_id};
use crate::specialization_context::SierraSignatureSpecializationContext;

/// See [SierraGenGroup::get_concrete_type_id] for documentation.
#[salsa::tracked(returns(ref))]
pub fn get_concrete_type_id<'db>(
    db: &'db dyn Database,
    type_id: semantic::TypeId<'db>,
) -> Maybe<cairo_lang_sierra::ids::ConcreteTypeId> {
    match type_id.long(db) {
        semantic::TypeLongId::Snapshot(inner_ty) => {
            let inner = db.get_concrete_type_id(*inner_ty)?;
            if matches!(
                db.lookup_concrete_type(inner),
                SierraGeneratorTypeLongId::CycleBreaker(ty) if cycle_breaker_info(db, ty)?.duplicatable
            ) {
                return Ok(inner.clone());
            }
        }
        semantic::TypeLongId::Concrete(
            semantic::ConcreteTypeId::Enum(_) | semantic::ConcreteTypeId::Struct(_),
        ) if db.is_self_referential(type_id)? => {
            return Ok(db.intern_concrete_type(SierraGeneratorTypeLongId::CycleBreaker(type_id)));
        }
        _ => {
            if type_id.is_phantom(db) {
                return Ok(db.intern_concrete_type(SierraGeneratorTypeLongId::Phantom(type_id)));
            }
        }
    }
    Ok(db.intern_concrete_type(SierraGeneratorTypeLongId::Regular(
        db.get_concrete_long_type_id(type_id)?.clone(),
    )))
}

/// See [SierraGenGroup::get_index_enum_type_id] for documentation.
#[salsa::tracked(returns(ref))]
pub fn get_index_enum_type_id(
    db: &dyn Database,
    _tracked: Tracked,
    index_count: usize,
) -> Maybe<cairo_lang_sierra::ids::ConcreteTypeId> {
    let unit_ty_arg = SierraGenericArg::Type(
        db.get_concrete_type_id(semantic::TypeLongId::Tuple(vec![]).intern(db))?.clone(),
    );
    let generic_args = chain!(
        [SierraGenericArg::UserType(format!("index_enum_type<{index_count}>").into())],
        itertools::repeat_n(unit_ty_arg, index_count)
    )
    .collect();
    let x = SierraGeneratorTypeLongId::Regular(
        ConcreteTypeLongId { generic_id: "Enum".into(), generic_args }.into(),
    );

    Ok(db.intern_concrete_type(x))
}

/// See [SierraGenGroup::get_concrete_long_type_id] for documentation.
#[salsa::tracked(returns(ref))]
pub fn get_concrete_long_type_id<'db>(
    db: &'db dyn Database,
    type_id: semantic::TypeId<'db>,
) -> Maybe<Arc<cairo_lang_sierra::program::ConcreteTypeLongId>> {
    let user_type_long_id = |generic_id: &str, user_type: UserTypeId| {
        let deps = db.type_dependencies(type_id)?;
        Ok(ConcreteTypeLongId {
            generic_id: generic_id.into(),
            generic_args: chain!(
                [Ok(SierraGenericArg::UserType(user_type))],
                deps.iter().map(|generic_arg_ty| db
                    .get_concrete_type_id(*generic_arg_ty)
                    .cloned()
                    .map(SierraGenericArg::Type))
            )
            .collect::<Maybe<_>>()?,
        })
    };
    Ok(match type_id.long(db) {
        semantic::TypeLongId::Concrete(ty) => {
            match ty {
                semantic::ConcreteTypeId::Struct(_) => {
                    user_type_long_id("Struct", ty.format(db).into())?.into()
                }
                semantic::ConcreteTypeId::Enum(_) => {
                    user_type_long_id("Enum", ty.format(db).into())?.into()
                }
                semantic::ConcreteTypeId::Extern(extrn) => {
                    ConcreteTypeLongId {
                        // TODO(Gil): Implement name for semantic::ConcreteTypeId
                        generic_id: extrn.extern_type_id(db).name(db).long(db).clone().into(),
                        generic_args: ty
                            .generic_args(db)
                            .into_iter()
                            .map(|arg| match arg {
                                semantic::GenericArgumentId::Type(ty) => SierraGenericArg::Type(
                                    db.get_concrete_type_id(ty).unwrap().clone(),
                                ),
                                semantic::GenericArgumentId::Constant(value_id) => {
                                    SierraGenericArg::Value(
                                        value_id
                                            .long(db)
                                            .to_int()
                                            .expect("Expected ConstValue::Int for size")
                                            .clone(),
                                    )
                                }
                                semantic::GenericArgumentId::Impl(_) => {
                                    panic!("Extern function with impl generics are not supported.")
                                }
                                semantic::GenericArgumentId::NegImpl(_) => panic!(
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
            let inner_ty = db.get_concrete_type_id(*ty).unwrap();
            let ty =
                snapshot_ty(&SierraSignatureSpecializationContext(db), inner_ty.clone()).unwrap();
            if ty == *inner_ty {
                return sierra_concrete_long_id(db, ty.clone());
            } else {
                ConcreteTypeLongId {
                    generic_id: "Snapshot".into(),
                    generic_args: vec![SierraGenericArg::Type(inner_ty.clone())],
                }
                .into()
            }
        }
        semantic::TypeLongId::Coupon(function_id) => ConcreteTypeLongId {
            generic_id: "Coupon".into(),
            generic_args: vec![SierraGenericArg::UserFunc(
                db.intern_sierra_function(function_id.lowered(db)),
            )],
        }
        .into(),
        semantic::TypeLongId::GenericParameter(_)
        | semantic::TypeLongId::Var(_)
        | semantic::TypeLongId::ImplType(_)
        | semantic::TypeLongId::Missing(_) => {
            panic!("Types should be fully resolved at this point. Got: `{}`.", type_id.format(db))
        }
        semantic::TypeLongId::Closure(_) => {
            user_type_long_id("Struct", (type_id.format(db)).into())?.into()
        }
    })
}

/// See [SierraGenGroup::is_self_referential] for documentation.
#[salsa::tracked]
pub fn is_self_referential<'db>(
    db: &'db dyn Database,
    type_id: semantic::TypeId<'db>,
) -> Maybe<bool> {
    db.has_in_deps(type_id, type_id)
}

/// See [SierraGenGroup::type_dependencies] for documentation.
#[salsa::tracked(returns(ref))]
pub fn type_dependencies<'db>(
    db: &'db dyn Database,
    type_id: semantic::TypeId<'db>,
) -> Maybe<Vec<semantic::TypeId<'db>>> {
    Ok(match type_id.long(db) {
        semantic::TypeLongId::Concrete(ty) => match ty {
            semantic::ConcreteTypeId::Struct(structure) => db
                .concrete_struct_members(*structure)?
                .iter()
                .map(|(_, member)| member.ty)
                .collect(),
            semantic::ConcreteTypeId::Enum(enm) => {
                db.concrete_enum_variants(*enm)?.into_iter().map(|variant| variant.ty).collect()
            }
            semantic::ConcreteTypeId::Extern(_extrn) => ty
                .generic_args(db)
                .into_iter()
                .filter_map(|arg| try_extract_matches!(arg, semantic::GenericArgumentId::Type))
                .collect(),
        },
        semantic::TypeLongId::Tuple(inner_types) => inner_types.clone(),
        semantic::TypeLongId::Snapshot(ty) => vec![*ty],
        semantic::TypeLongId::Coupon(_) => vec![],
        semantic::TypeLongId::Closure(closure_ty) => closure_ty.captured_types.clone(),
        semantic::TypeLongId::FixedSizeArray { type_id, size } => {
            let size = size
                .long(db)
                .to_int()
                .expect("Expected ConstValue::Int for size")
                .to_usize()
                .unwrap();
            [*type_id].repeat(size)
        }
        semantic::TypeLongId::GenericParameter(_)
        | semantic::TypeLongId::Var(_)
        | semantic::TypeLongId::ImplType(_)
        | semantic::TypeLongId::Missing(_) => {
            panic!("Types should be fully resolved at this point. Got: `{}`.", type_id.format(db))
        }
    })
}

/// See [SierraGenGroup::has_in_deps] for documentation.
#[salsa::tracked(cycle_result=has_in_deps_cycle)]
pub fn has_in_deps<'db>(
    db: &'db dyn Database,
    type_id: semantic::TypeId<'db>,
    needle: semantic::TypeId<'db>,
) -> Maybe<bool> {
    let deps = db.type_dependencies(type_id)?;
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
pub fn has_in_deps_cycle<'db>(
    _db: &'db dyn Database,
    _type_id: semantic::TypeId<'db>,
    _needle: semantic::TypeId<'db>,
) -> Maybe<bool> {
    Ok(false)
}

/// Information about a cycle breaker type.
pub struct CycleBreakerTypeInfo {
    /// Is the type duplicatable in Sierra - it is considered it is so if it is `Copy` or all its
    /// dependencies are `Copy`.
    pub duplicatable: bool,
    /// Is the type droppable in Sierra - it is considered it is so if it is `Drop` or all its
    /// dependencies are `Drop`.
    pub droppable: bool,
}

/// Returns the approximation of `ty`s droppable and copyable traits.
///
/// Assumes the type is a cycle breaker.
pub fn cycle_breaker_info<'db>(
    db: &dyn Database,
    ty: semantic::TypeId<'db>,
) -> Maybe<CycleBreakerTypeInfo> {
    let mut duplicatable = db.copyable(ty).is_ok();
    let mut droppable = db.droppable(ty).is_ok();
    if !duplicatable || !droppable {
        let deps = db.type_dependencies(ty)?;
        duplicatable = duplicatable || deps.iter().all(|dep| db.copyable(*dep).is_ok());
        droppable = droppable || deps.iter().all(|dep| db.droppable(*dep).is_ok());
    }
    Ok(CycleBreakerTypeInfo { duplicatable, droppable })
}
