use std::sync::Arc;

use cairo_lang_defs::ids::{LanguageElementId, LookupItemId, ModuleItemId, ModuleTypeAliasId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};

use super::generics::GenericParamsData;
use super::type_aliases::{
    type_alias_generic_params_data_helper, type_alias_semantic_data_cycle_helper,
    type_alias_semantic_data_helper, TypeAliasData,
};
use crate::db::SemanticGroup;
use crate::resolve::ResolverData;
use crate::{GenericParam, SemanticDiagnostic, TypeId};

// --- Selectors ---

/// Query implementation of [crate::db::SemanticGroup::module_type_alias_semantic_diagnostics].
pub fn module_type_alias_semantic_diagnostics(
    db: &dyn SemanticGroup,
    module_type_alias_id: ModuleTypeAliasId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_module_type_alias_semantic_data(module_type_alias_id)
        .map(|data| data.diagnostics)
        .unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::module_type_alias_resolved_type].
pub fn module_type_alias_resolved_type(
    db: &dyn SemanticGroup,
    module_type_alias_id: ModuleTypeAliasId,
) -> Maybe<TypeId> {
    db.priv_module_type_alias_semantic_data(module_type_alias_id)?.resolved_type
}

/// Query implementation of [crate::db::SemanticGroup::module_type_alias_generic_params].
pub fn module_type_alias_generic_params(
    db: &dyn SemanticGroup,
    module_type_alias_id: ModuleTypeAliasId,
) -> Maybe<Vec<GenericParam>> {
    Ok(db.priv_module_type_alias_generic_params_data(module_type_alias_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::module_type_alias_resolver_data].
pub fn module_type_alias_resolver_data(
    db: &dyn SemanticGroup,
    module_type_alias_id: ModuleTypeAliasId,
) -> Maybe<Arc<ResolverData>> {
    Ok(db.priv_module_type_alias_semantic_data(module_type_alias_id)?.resolver_data)
}

// --- Computation ---

/// Query implementation of [crate::db::SemanticGroup::priv_module_type_alias_semantic_data].
pub fn priv_module_type_alias_semantic_data(
    db: &(dyn SemanticGroup),
    module_type_alias_id: ModuleTypeAliasId,
) -> Maybe<TypeAliasData> {
    let module_file_id = module_type_alias_id.module_file_id(db.upcast());
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let module_type_aliases = db.module_type_aliases(module_file_id.0)?;
    let module_type_alias_ast = module_type_aliases.get(&module_type_alias_id).to_maybe()?;
    let generic_params_data =
        db.priv_module_type_alias_generic_params_data(module_type_alias_id)?;
    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::TypeAlias(module_type_alias_id));

    type_alias_semantic_data_helper(
        db,
        module_file_id,
        module_type_alias_ast,
        lookup_item_id,
        generic_params_data,
    )
}

/// Cycle handling for [crate::db::SemanticGroup::priv_module_type_alias_semantic_data].
pub fn priv_module_type_alias_semantic_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    module_type_alias_id: &ModuleTypeAliasId,
) -> Maybe<TypeAliasData> {
    let module_file_id = module_type_alias_id.module_file_id(db.upcast());
    let module_type_aliases = db.module_type_aliases(module_file_id.0)?;
    let type_alias_ast = module_type_aliases.get(module_type_alias_id).to_maybe()?;
    let generic_params_data =
        db.priv_module_type_alias_generic_params_data(*module_type_alias_id)?;
    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::TypeAlias(*module_type_alias_id));

    type_alias_semantic_data_cycle_helper(
        db,
        module_file_id,
        type_alias_ast,
        lookup_item_id,
        generic_params_data,
    )
}

/// Query implementation of [crate::db::SemanticGroup::priv_module_type_alias_generic_params_data].
pub fn priv_module_type_alias_generic_params_data(
    db: &dyn SemanticGroup,
    module_type_alias_id: ModuleTypeAliasId,
) -> Maybe<GenericParamsData> {
    let module_file_id = module_type_alias_id.module_file_id(db.upcast());
    let type_alias_ast = db.module_type_alias_by_id(module_type_alias_id)?.to_maybe()?;
    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::TypeAlias(module_type_alias_id));

    type_alias_generic_params_data_helper(db, module_file_id, &type_alias_ast, lookup_item_id, None)
}
