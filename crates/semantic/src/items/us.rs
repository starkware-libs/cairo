use std::sync::Arc;

use defs::ids::{LanguageElementId, UseId};
use diagnostics::Diagnostics;
use diagnostics_proc_macros::DebugWithDb;

use crate::db::SemanticGroup;
use crate::diagnostic::{SemanticDiagnosticKind, SemanticDiagnostics};
use crate::resolve_path::{ResolvedGenericItem, ResolvedLookback, Resolver};
use crate::SemanticDiagnostic;

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct UseData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    pub resolved_item: Option<ResolvedGenericItem>,
    resolved_lookback: Arc<ResolvedLookback>,
}

/// Query implementation of [crate::db::SemanticGroup::priv_struct_semantic_data].
pub fn priv_use_semantic_data(db: &(dyn SemanticGroup), use_id: UseId) -> Option<UseData> {
    // TODO(spapini): When asts are rooted on items, don't query module_data directly. Use a
    // selector.
    let module_file_id = use_id.module_file(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    // TODO(spapini): Add generic args when they are supported on structs.
    let mut resolver = Resolver::new(db, module_file_id, &[]);
    let module_data = db.module_data(module_file_id.0)?;
    let use_ast = module_data.uses.get(&use_id)?;
    let syntax_db = db.upcast();
    let resolved_item = resolver.resolve_generic_path(&mut diagnostics, &use_ast.name(syntax_db));
    let resolved_lookback = Arc::new(resolver.lookback);
    Some(UseData { diagnostics: diagnostics.build(), resolved_item, resolved_lookback })
}

/// Cycle handling for [crate::db::SemanticGroup::priv_struct_semantic_data].
pub fn priv_use_semantic_data_cycle(
    db: &dyn SemanticGroup,
    _cycle: &[String],
    use_id: &UseId,
) -> Option<UseData> {
    let module_file_id = use_id.module_file(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    let module_data = db.module_data(module_file_id.0)?;
    let use_ast = module_data.uses.get(use_id)?;
    let syntax_db = db.upcast();
    diagnostics.report(&use_ast.name(syntax_db), SemanticDiagnosticKind::UseCycle);
    Some(UseData {
        diagnostics: diagnostics.build(),
        resolved_item: None,
        resolved_lookback: Arc::new(ResolvedLookback::default()),
    })
}

/// Query implementation of [crate::db::SemanticGroup::use_semantic_diagnostics].
pub fn use_semantic_diagnostics(
    db: &dyn SemanticGroup,
    use_id: UseId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_use_semantic_data(use_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::use_resolved_item].
pub fn use_resolved_item(db: &dyn SemanticGroup, use_id: UseId) -> Option<ResolvedGenericItem> {
    db.priv_use_semantic_data(use_id).and_then(|data| data.resolved_item)
}

/// Query implementation of [crate::db::SemanticGroup::use_resolved_lookback].
pub fn use_resolved_lookback(
    db: &dyn SemanticGroup,
    use_id: UseId,
) -> Option<Arc<ResolvedLookback>> {
    Some(db.priv_use_semantic_data(use_id)?.resolved_lookback)
}
