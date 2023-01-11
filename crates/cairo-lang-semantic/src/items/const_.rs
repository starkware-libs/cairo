use cairo_lang_defs::ids::{ConstId, LanguageElementId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::SemanticDiagnostic;

// TODO(lior): Add a test.
// #[cfg(test)]
// #[path = "const_test.rs"]
// mod test;

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ConstData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
}

/// Query implementation of [crate::db::SemanticGroup::const_semantic_diagnostics].
pub fn const_semantic_diagnostics(
    db: &dyn SemanticGroup,
    const_id: ConstId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_const_semantic_data(const_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::priv_const_semantic_data].
pub fn priv_const_semantic_data(db: &dyn SemanticGroup, const_id: ConstId) -> Maybe<ConstData> {
    let module_file_id = const_id.module_file(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_file_id);
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    let module_constants = db.module_constants(module_file_id.0)?;
    let const_ast = module_constants.get(&const_id).to_maybe()?;
    let syntax_db = db.upcast();

    // TODO(lior): Implement constants.
    diagnostics.report(&const_ast.const_kw(syntax_db), ConstantsAreNotSupported);

    Ok(ConstData { diagnostics: diagnostics.build() })
}
