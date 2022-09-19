use defs::ids::{ModuleId, ModuleItemId};
use syntax::node::ast::{self, PathSegment};

use crate::corelib::core_module;
use crate::db::SemanticGroup;

pub fn resolve_item(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    path: &ast::ExprPath,
) -> Option<ModuleItemId> {
    let syntax_db = db.upcast();
    let elements = path.elements(syntax_db);
    if elements.len() != 1 {
        // TODO(spapini): Qualified paths are not supported yet.
        return None;
    }

    match &elements[0] {
        PathSegment::Ident(ident_segment) => {
            let name = ident_segment.ident(syntax_db).text(syntax_db);
            db.module_item_by_name(module_id, name.clone())
                .or_else(|| db.module_item_by_name(core_module(db), name))
        }
        PathSegment::GenericArgs(_generic_args_segment) => {
            // TODO(ilya, 10/10/2022): Generics are not supported yet.
            None
        }
    }
}
