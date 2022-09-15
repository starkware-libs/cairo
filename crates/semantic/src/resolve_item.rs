use defs::ids::{ModuleId, ModuleItemId};
use syntax::node::ast;

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
        todo!("Qualified paths are not supported yet");
    }
    let last_element = &elements[0];
    // TODO(spapini): Support generics.
    if let ast::OptionGenericArgs::Some(_) = last_element.generic_args(syntax_db) {
        todo!("Generics are not supported yet")
    };
    let name = last_element.ident(syntax_db).text(syntax_db);
    db.module_item_by_name(module_id, name.clone())
        .or_else(|| db.module_item_by_name(core_module(db), name))
}
