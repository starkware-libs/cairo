use cairo_lang_defs::ids::{GenericParamId, GenericParamLongId, ModuleFileId};
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnostics;

/// Returns the parameters of the given function signature's AST.
pub fn semantic_generic_params(
    db: &dyn SemanticGroup,
    _diagnostics: &mut SemanticDiagnostics,
    module_file_id: ModuleFileId,
    generic_params: &ast::OptionWrappedGenericParamList,
) -> Vec<GenericParamId> {
    let syntax_db = db.upcast();

    match generic_params {
        syntax::node::ast::OptionWrappedGenericParamList::Empty(_) => vec![],
        syntax::node::ast::OptionWrappedGenericParamList::WrappedGenericParamList(syntax) => syntax
            .generic_params(syntax_db)
            .elements(syntax_db)
            .iter()
            .map(|param_syntax| {
                db.intern_generic_param(GenericParamLongId(
                    module_file_id,
                    param_syntax.stable_ptr(),
                ))
            })
            .collect(),
    }
}
