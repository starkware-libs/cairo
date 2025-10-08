use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{LanguageElementId, LookupItemId, ModuleItemId, ModuleTypeAliasId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, MaybeAsRef, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;
use salsa::Database;

use super::generics::GenericParamsData;
use super::type_aliases::{
    TypeAliasData, type_alias_generic_params_data_helper, type_alias_semantic_data_cycle_helper,
    type_alias_semantic_data_helper,
};
use crate::diagnostic::SemanticDiagnostics;
use crate::resolve::ResolverData;
use crate::{GenericParam, SemanticDiagnostic, TypeId};

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
struct ModuleTypeAliasData<'db> {
    type_alias_data: TypeAliasData<'db>,
    /// The diagnostics of the module type alias, including the ones for the type alias itself.
    diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
}

/// Returns the data of a type alias.
#[salsa::tracked(cycle_result=module_type_alias_semantic_data_cycle, returns(ref))]
fn module_type_alias_semantic_data<'db>(
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
        module_type_alias_generic_params_data(db, module_type_alias_id).maybe_as_ref()?.clone();
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

/// Cycle handling for [module_type_alias_semantic_data].
fn module_type_alias_semantic_data_cycle<'db>(
    db: &'db dyn Database,
    module_type_alias_id: ModuleTypeAliasId<'db>,
    _in_cycle: bool,
) -> Maybe<ModuleTypeAliasData<'db>> {
    module_type_alias_semantic_data(db, module_type_alias_id, true).clone()
}

/// Returns the generic parameters data of a type alias.
#[salsa::tracked(returns(ref))]
fn module_type_alias_generic_params_data<'db>(
    db: &'db dyn Database,
    module_type_alias_id: ModuleTypeAliasId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    let module_id = module_type_alias_id.module_id(db);
    let type_alias_ast = db.module_type_alias_by_id(module_type_alias_id)?;
    let lookup_item_id = LookupItemId::ModuleItem(ModuleItemId::TypeAlias(module_type_alias_id));

    type_alias_generic_params_data_helper(db, module_id, &type_alias_ast, lookup_item_id, None)
}

/// Trait for module type alias-related semantic queries.
pub trait ModuleTypeAliasSemantic<'db>: Database {
    /// Returns the semantic diagnostics of a type alias.
    fn module_type_alias_semantic_diagnostics(
        &'db self,
        id: ModuleTypeAliasId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        module_type_alias_semantic_data(self.as_dyn_database(), id, false)
            .as_ref()
            .map(|data| data.diagnostics.clone())
            .unwrap_or_default()
    }
    /// Returns the resolved type of a type alias.
    fn module_type_alias_resolved_type(
        &'db self,
        id: ModuleTypeAliasId<'db>,
    ) -> Maybe<TypeId<'db>> {
        module_type_alias_semantic_data(self.as_dyn_database(), id, false)
            .maybe_as_ref()?
            .type_alias_data
            .resolved_type
    }
    /// Returns the generic parameters of a type alias.
    fn module_type_alias_generic_params(
        &'db self,
        id: ModuleTypeAliasId<'db>,
    ) -> Maybe<Vec<GenericParam<'db>>> {
        Ok(module_type_alias_generic_params_data(self.as_dyn_database(), id)
            .maybe_as_ref()?
            .generic_params
            .clone())
    }
    /// Returns the resolution resolved_items of a type alias.
    fn module_type_alias_resolver_data(
        &'db self,
        id: ModuleTypeAliasId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>> {
        Ok(module_type_alias_semantic_data(self.as_dyn_database(), id, false)
            .maybe_as_ref()?
            .type_alias_data
            .resolver_data
            .clone())
    }
}
impl<'db, T: Database + ?Sized> ModuleTypeAliasSemantic<'db> for T {}
