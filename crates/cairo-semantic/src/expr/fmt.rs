use cairo_defs::db::DefsGroup;
use cairo_defs::ids::FreeFunctionId;
use cairo_utils::Upcast;

use crate::db::SemanticGroup;

/// Holds all the information needed for formatting expressions.
/// Acts like a "db" for DebugWithDb.
pub struct ExprFormatter<'a> {
    pub db: &'a (dyn SemanticGroup + 'static),
    pub free_function_id: FreeFunctionId,
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
