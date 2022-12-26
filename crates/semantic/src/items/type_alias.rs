use std::sync::Arc;

use defs::ids::{GenericParamId, LanguageElementId, TypeAliasId};
use diagnostics::{Diagnostics, Maybe, ToMaybe};
use diagnostics_proc_macros::DebugWithDb;

use super::generics::semantic_generic_params;
use crate::db::SemanticGroup;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics};
use crate::resolve_path::{ResolvedLookback, Resolver};
use crate::types::resolve_type;
use crate::{SemanticDiagnostic, TypeId};

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct TypeAliasData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    resolved_type: Maybe<TypeId>,
    generic_params: Vec<GenericParamId>,
    resolved_lookback: Arc<ResolvedLookback>,
}

/// Query implementation of [crate::db::SemanticGroup::priv_struct_semantic_data].
pub fn priv_type_alias_semantic_data(
    db: &(dyn SemanticGroup),
    type_alias_id: TypeAliasId,
) -> Maybe<TypeAliasData> {
    // TODO(spapini): When asts are rooted on items, don't query module_data directly. Use a
    // selector.
    let module_file_id = type_alias_id.module_file(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    // TODO(spapini): Add generic args when they are supported on structs.
    let module_data = db.module_data(module_file_id.0)?;
    let type_alias_ast = module_data.type_aliases.get(&type_alias_id).to_maybe()?;
    let syntax_db = db.upcast();
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        module_file_id,
        &type_alias_ast.generic_params(db.upcast()),
    );
    let mut resolver = Resolver::new(db, module_file_id, &generic_params);
    let ty = resolve_type(db, &mut diagnostics, &mut resolver, &type_alias_ast.ty(syntax_db));
    let resolved_lookback = Arc::new(resolver.lookback);
    Ok(TypeAliasData {
        diagnostics: diagnostics.build(),
        resolved_type: Ok(ty),
        generic_params,
        resolved_lookback,
    })
}

/// Cycle handling for [crate::db::SemanticGroup::priv_struct_semantic_data].
pub fn priv_type_alias_semantic_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    type_alias_id: &TypeAliasId,
) -> Maybe<TypeAliasData> {
    let module_file_id = type_alias_id.module_file(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    let module_data = db.module_data(module_file_id.0)?;
    let type_alias_ast = module_data.type_aliases.get(type_alias_id).to_maybe()?;
    let syntax_db = db.upcast();
    let err =
        Err(diagnostics
            .report(&type_alias_ast.name(syntax_db), SemanticDiagnosticKind::TypeAliasCycle));
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        module_file_id,
        &type_alias_ast.generic_params(db.upcast()),
    );
    Ok(TypeAliasData {
        diagnostics: diagnostics.build(),
        resolved_type: err,
        generic_params,
        resolved_lookback: Arc::new(ResolvedLookback::default()),
    })
}

/// Query implementation of [crate::db::SemanticGroup::type_alias_semantic_diagnostics].
pub fn type_alias_semantic_diagnostics(
    db: &dyn SemanticGroup,
    type_alias_id: TypeAliasId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_type_alias_semantic_data(type_alias_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::type_alias_resolved_type].
pub fn type_alias_resolved_type(
    db: &dyn SemanticGroup,
    type_alias_id: TypeAliasId,
) -> Maybe<TypeId> {
    db.priv_type_alias_semantic_data(type_alias_id)?.resolved_type
}

/// Query implementation of [crate::db::SemanticGroup::type_alias_generic_params].
pub fn type_alias_generic_params(
    db: &dyn SemanticGroup,
    type_alias_id: TypeAliasId,
) -> Maybe<Vec<GenericParamId>> {
    Ok(db.priv_type_alias_semantic_data(type_alias_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::type_alias_resolved_lookback].
pub fn type_alias_resolved_lookback(
    db: &dyn SemanticGroup,
    type_alias_id: TypeAliasId,
) -> Maybe<Arc<ResolvedLookback>> {
    Ok(db.priv_type_alias_semantic_data(type_alias_id)?.resolved_lookback)
}
