use defs::ids::ModuleItemId;
use diagnostics::{Diagnostics, WithDiagnostics};
use diagnostics_proc_macros::with_diagnostics;
use filesystem::ids::ModuleId;
use syntax::node::ast;

use crate::corelib::core_module;
use crate::db::SemanticGroup;
use crate::Diagnostic;

#[with_diagnostics]
pub fn resolve_item(
    diagnostics: &mut Diagnostics<Diagnostic>,
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    path: &ast::ExprPath,
) -> Option<ModuleItemId> {
    let syntax_db = db.as_syntax_group();
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
        .propagte(diagnostics)
        .or_else(|| db.module_item_by_name(core_module(db), name).propagte(diagnostics))
}
