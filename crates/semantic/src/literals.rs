#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LiteralLongId {
    pub text: smol_str::SmolStr,
}

use db_utils::define_short_id;

use crate::db::SemanticGroup;

define_short_id!(LiteralId, LiteralLongId, SemanticGroup, lookup_intern_literal);
impl LiteralId {
    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        db.lookup_intern_literal(*self).text.to_string()
    }
}
