use cairo_lang_defs::ids::{ConstantId, LanguageElementId};
use cairo_lang_diagnostics::{Diagnostics, Maybe, ToMaybe};
use cairo_lang_proc_macros::DebugWithDb;

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::SemanticDiagnostic;

#[cfg(test)]
#[path = "constant_test.rs"]
mod test;

/// Information about a constant definition.
///
/// Helper struct for the data returned by [SemanticGroup::priv_constant_semantic_data].
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ConstantData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
}

/// Query implementation of [SemanticGroup::constant_semantic_diagnostics].
pub fn constant_semantic_diagnostics(
    db: &dyn SemanticGroup,
    const_id: ConstantId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_constant_semantic_data(const_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [SemanticGroup::priv_constant_semantic_data].
pub fn priv_constant_semantic_data(
    db: &dyn SemanticGroup,
    const_id: ConstantId,
) -> Maybe<ConstantData> {
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

    Ok(ConstantData { diagnostics: diagnostics.build() })
}
