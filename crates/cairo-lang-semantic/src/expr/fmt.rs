use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::FunctionWithBodyId;
use cairo_lang_utils::Upcast;

use crate::db::SemanticGroup;

/// Holds all the information needed for formatting expressions.
/// Acts like a "db" for DebugWithDb.
pub struct ExprFormatter<'a> {
    pub db: &'a (dyn SemanticGroup + 'static),
    pub function_id: FunctionWithBodyId,
}

impl<'a> Upcast<dyn SemanticGroup + 'static> for ExprFormatter<'a> {
    fn upcast(&self) -> &(dyn SemanticGroup + 'static) {
        self.db
    }
}
impl<'a> Upcast<dyn DefsGroup + 'static> for ExprFormatter<'a> {
    fn upcast(&self) -> &(dyn DefsGroup + 'static) {
        self.db.upcast()
    }
}
