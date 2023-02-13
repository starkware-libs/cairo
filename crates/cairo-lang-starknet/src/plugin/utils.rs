use cairo_lang_syntax::node::ast::{self, Modifier};
use cairo_lang_syntax::node::db::SyntaxGroup;

/// Checks if the parameter is defined as a ref parameter.
pub fn is_ref_param(db: &dyn SyntaxGroup, param: &ast::Param) -> bool {
    let param_modifiers = param.modifiers(db).elements(db);
    // TODO(yuval): This works only if "ref" is the only modifier. If the expansion was at the
    // semantic level, we could just ask if it's a reference.
    param_modifiers.len() == 1 && matches!(param_modifiers[0], Modifier::Ref(_))
}
