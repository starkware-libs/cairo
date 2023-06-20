use cairo_lang_defs::ids::ModuleId;
use cairo_lang_syntax::node::ast;
use crate::db::SemanticGroup;

pub fn visible_in(db: &dyn SemanticGroup, visibility: &ast::Visibility, source_module_id: ModuleId, module_id: ModuleId) -> bool {
    if source_module_id == module_id {
        return true
    }
    match visibility {
        ast::Visibility::Public(_) => true,
        ast::Visibility::Default(_) => db.module_ancestors(module_id).contains(&source_module_id),
    }
}
