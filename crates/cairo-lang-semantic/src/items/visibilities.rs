use cairo_lang_defs::ids::ModuleId;
use cairo_lang_syntax::node::ast;

use crate::db::SemanticGroup;

/// Determine whether source module is visible to target module given the ast visibility,
/// ignoring or forgetting the visibility of the ancestors of source module for a moment.
pub fn peek_visible_in(
    db: &dyn SemanticGroup,
    visibility: &ast::Visibility,
    source_module_id: ModuleId,
    module_id: ModuleId,
) -> bool {
    if source_module_id == module_id {
        return true;
    }
    match visibility {
        ast::Visibility::Public(_) => true,
        ast::Visibility::Default(_) => db.module_ancestors(module_id).contains(&source_module_id),
    }
}
