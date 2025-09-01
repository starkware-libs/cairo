use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{LanguageElementId, LookupItemId, ModuleItemId, ModuleTypeAliasId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;

use super::generics::GenericParamsData;
use super::type_aliases::{
    TypeAliasData, type_alias_generic_params_data_helper, type_alias_semantic_data_cycle_helper,
    type_alias_semantic_data_helper,
};
use crate::db::{SemanticGroup, SemanticGroupData};
use crate::diagnostic::SemanticDiagnostics;
use crate::resolve::ResolverData;
use crate::{GenericParam, SemanticDiagnostic, TypeId};

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn SemanticGroup)]
pub struct ModuleTypeAliasData<'db> {
    pub type_alias_data: TypeAliasData<'db>,
    /// The diagnostics of the module type alias, including the ones for the type alias itself.
    diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
}

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::module_type_alias_semantic_diagnostics].
pub fn module_type_alias_semantic_diagnostics<'db>(
    db: &'db dyn SemanticGroup,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    db.priv_module_type_alias_semantic_data(module_type_alias_id, false)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::module_type_alias_resolved_type].
pub fn module_type_alias_resolved_type<'db>(
    db: &'db dyn SemanticGroup,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Maybe<TypeId<'db>> {
    db.priv_module_type_alias_semantic_data(module_type_alias_id, false)?
        .type_alias_data
        .resolved_type
}

/// Trivial cycle handling for [crate::db::SemanticGroup::module_type_alias_resolved_type].
pub fn module_type_alias_resolved_type_cycle<'db>(
    db: &'db dyn SemanticGroup,
    _input: SemanticGroupData,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Maybe<TypeId<'db>> {
    // Forwarding (not as a query) cycle handling to `priv_module_type_alias_semantic_data` cycle
    // handler.
    db.priv_module_type_alias_semantic_data(module_type_alias_id, true)?
        .type_alias_data
        .resolved_type
}

/// Query implementation of [crate::db::SemanticGroup::module_type_alias_generic_params].
pub fn module_type_alias_generic_params<'db>(
    db: &'db dyn SemanticGroup,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Maybe<Vec<GenericParam<'db>>> {
    Ok(db.priv_module_type_alias_generic_params_data(module_type_alias_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::module_type_alias_resolver_data].
pub fn module_type_alias_resolver_data<'db>(
    db: &'db dyn SemanticGroup,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    Ok(db
        .priv_module_type_alias_semantic_data(module_type_alias_id, false)?
        .type_alias_data
        .resolver_data)
}

/// Trivial cycle handling for [crate::db::SemanticGroup::module_type_alias_resolver_data].
pub fn module_type_alias_resolver_data_cycle<'db>(
    db: &'db dyn SemanticGroup,
    _input: SemanticGroupData,
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

/// Query implementation of [crate::db::SemanticGroup::priv_module_type_alias_semantic_data].
pub fn priv_module_type_alias_semantic_data<'db>(
    db: &'db dyn SemanticGroup,
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

/// Cycle handling for [crate::db::SemanticGroup::priv_module_type_alias_semantic_data].
pub fn priv_module_type_alias_semantic_data_cycle<'db>(
    db: &'db dyn SemanticGroup,
    _input: SemanticGroupData,
    module_type_alias_id: ModuleTypeAliasId<'db>,
    _in_cycle: bool,
) -> Maybe<ModuleTypeAliasData<'db>> {
    db.priv_module_type_alias_semantic_data(module_type_alias_id, true)
}

/// Query implementation of [crate::db::SemanticGroup::priv_module_type_alias_generic_params_data].
pub fn priv_module_type_alias_generic_params_data<'db>(
    db: &'db dyn SemanticGroup,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    let module_file_id = module_type_alias_id.module_file_id(db);
    let type_alias_ast = db.module_type_alias_by_id(module_type_alias_id)?;
    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::TypeAlias(module_type_alias_id));

    type_alias_generic_params_data_helper(db, module_file_id, &type_alias_ast, lookup_item_id, None)
}
