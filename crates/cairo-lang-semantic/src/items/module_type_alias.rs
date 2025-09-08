use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{LanguageElementId, LookupItemId, ModuleItemId, ModuleTypeAliasId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;
use salsa::Database;

use super::generics::GenericParamsData;
use super::type_aliases::{
    TypeAliasData, type_alias_generic_params_data_helper, type_alias_semantic_data_cycle_helper,
    type_alias_semantic_data_helper,
};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnostics;
use crate::resolve::ResolverData;
use crate::{GenericParam, SemanticDiagnostic, TypeId};

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
pub struct ModuleTypeAliasData<'db> {
    pub type_alias_data: TypeAliasData<'db>,
    /// The diagnostics of the module type alias, including the ones for the type alias itself.
    diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
}

// --- Selectors ---

/// Implementation of [crate::db::SemanticGroup::module_type_alias_semantic_diagnostics].
pub fn module_type_alias_semantic_diagnostics<'db>(
    db: &'db dyn Database,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    db.priv_module_type_alias_semantic_data(module_type_alias_id, false)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::module_type_alias_semantic_diagnostics].
#[salsa::tracked]
pub fn module_type_alias_semantic_diagnostics_tracked<'db>(
    db: &'db dyn Database,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    module_type_alias_semantic_diagnostics(db, module_type_alias_id)
}

/// Implementation of [crate::db::SemanticGroup::module_type_alias_resolved_type].
pub fn module_type_alias_resolved_type<'db>(
    db: &'db dyn Database,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Maybe<TypeId<'db>> {
    db.priv_module_type_alias_semantic_data(module_type_alias_id, false)?
        .type_alias_data
        .resolved_type
}

/// Query implementation of [crate::db::SemanticGroup::module_type_alias_resolved_type].
#[salsa::tracked(cycle_result=module_type_alias_resolved_type_cycle)]
pub fn module_type_alias_resolved_type_tracked<'db>(
    db: &'db dyn Database,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Maybe<TypeId<'db>> {
    module_type_alias_resolved_type(db, module_type_alias_id)
}

/// Trivial cycle handling for [crate::db::SemanticGroup::module_type_alias_resolved_type].
pub fn module_type_alias_resolved_type_cycle<'db>(
    db: &'db dyn Database,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Maybe<TypeId<'db>> {
    // Forwarding (not as a query) cycle handling to `priv_module_type_alias_semantic_data` cycle
    // handler.
    db.priv_module_type_alias_semantic_data(module_type_alias_id, true)?
        .type_alias_data
        .resolved_type
}

/// Implementation of [crate::db::SemanticGroup::module_type_alias_generic_params].
pub fn module_type_alias_generic_params<'db>(
    db: &'db dyn Database,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Maybe<Vec<GenericParam<'db>>> {
    Ok(db.priv_module_type_alias_generic_params_data(module_type_alias_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::module_type_alias_generic_params].
#[salsa::tracked]
pub fn module_type_alias_generic_params_tracked<'db>(
    db: &'db dyn Database,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Maybe<Vec<GenericParam<'db>>> {
    module_type_alias_generic_params(db, module_type_alias_id)
}

/// Implementation of [crate::db::SemanticGroup::module_type_alias_resolver_data].
pub fn module_type_alias_resolver_data<'db>(
    db: &'db dyn Database,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    Ok(db
        .priv_module_type_alias_semantic_data(module_type_alias_id, false)?
        .type_alias_data
        .resolver_data)
}

/// Query implementation of [crate::db::SemanticGroup::module_type_alias_resolver_data].
#[salsa::tracked(cycle_result=module_type_alias_resolver_data_cycle)]
pub fn module_type_alias_resolver_data_tracked<'db>(
    db: &'db dyn Database,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    module_type_alias_resolver_data(db, module_type_alias_id)
}

/// Trivial cycle handling for [crate::db::SemanticGroup::module_type_alias_resolver_data].
pub fn module_type_alias_resolver_data_cycle<'db>(
    db: &'db dyn Database,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    // Forwarding (not as a query) cycle handling to `priv_module_type_alias_semantic_data` cycle
    // handler.
    Ok(db
        .priv_module_type_alias_semantic_data(module_type_alias_id, true)?
        .type_alias_data
        .resolver_data)
}

// --- Computation ---

/// Implementation of [crate::db::SemanticGroup::priv_module_type_alias_semantic_data].
pub fn priv_module_type_alias_semantic_data<'db>(
    db: &'db dyn Database,
    module_type_alias_id: ModuleTypeAliasId<'db>,
    in_cycle: bool,
) -> Maybe<ModuleTypeAliasData<'db>> {
    let module_id = module_type_alias_id.parent_module(db);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let module_type_aliases = module_id.module_data(db)?.type_aliases(db);
    let module_type_alias_ast = module_type_aliases.get(&module_type_alias_id).to_maybe()?;
    let generic_params_data =
        db.priv_module_type_alias_generic_params_data(module_type_alias_id)?;
    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::TypeAlias(module_type_alias_id));

    let mut diagnostics = SemanticDiagnostics::default();
    let type_alias_data = if in_cycle {
        type_alias_semantic_data_cycle_helper(
            db,
            &mut diagnostics,
            module_type_alias_ast,
            lookup_item_id,
            generic_params_data,
        )?
    } else {
        type_alias_semantic_data_helper(
            db,
            &mut diagnostics,
            module_type_alias_ast,
            lookup_item_id,
            generic_params_data,
        )?
    };
    Ok(ModuleTypeAliasData { type_alias_data, diagnostics: diagnostics.build() })
}

/// Query implementation of [crate::db::SemanticGroup::priv_module_type_alias_semantic_data].
#[salsa::tracked(cycle_result=priv_module_type_alias_semantic_data_cycle)]
pub fn priv_module_type_alias_semantic_data_tracked<'db>(
    db: &'db dyn Database,
    module_type_alias_id: ModuleTypeAliasId<'db>,
    in_cycle: bool,
) -> Maybe<ModuleTypeAliasData<'db>> {
    priv_module_type_alias_semantic_data(db, module_type_alias_id, in_cycle)
}

/// Cycle handling for [crate::db::SemanticGroup::priv_module_type_alias_semantic_data].
pub fn priv_module_type_alias_semantic_data_cycle<'db>(
    db: &'db dyn Database,
    module_type_alias_id: ModuleTypeAliasId<'db>,
    _in_cycle: bool,
) -> Maybe<ModuleTypeAliasData<'db>> {
    db.priv_module_type_alias_semantic_data(module_type_alias_id, true)
}

/// Implementation of [crate::db::SemanticGroup::priv_module_type_alias_generic_params_data].
pub fn priv_module_type_alias_generic_params_data<'db>(
    db: &'db dyn Database,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    let module_file_id = module_type_alias_id.module_file_id(db);
    let type_alias_ast = db.module_type_alias_by_id(module_type_alias_id)?;
    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::TypeAlias(module_type_alias_id));

    type_alias_generic_params_data_helper(db, module_file_id, &type_alias_ast, lookup_item_id, None)
}

/// Query implementation of [crate::db::SemanticGroup::priv_module_type_alias_generic_params_data].
#[salsa::tracked]
pub fn priv_module_type_alias_generic_params_data_tracked<'db>(
    db: &'db dyn Database,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    priv_module_type_alias_generic_params_data(db, module_type_alias_id)
}
