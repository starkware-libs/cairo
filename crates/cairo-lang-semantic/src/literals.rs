use cairo_lang_utils::define_short_id;
use num_bigint::BigInt;

use crate::db::SemanticGroup;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LiteralLongId {
    pub value: BigInt,
}

define_short_id!(LiteralId, LiteralLongId, SemanticGroup, lookup_intern_literal);
impl LiteralId {
    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        db.lookup_intern_literal(*self).value.to_string()
    }
}
