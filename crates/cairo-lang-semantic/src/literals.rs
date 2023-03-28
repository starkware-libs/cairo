use num_bigint::BigInt;
use num_traits::Num;
use smol_str::SmolStr;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LiteralLongId {
    pub value: BigInt,
}

impl TryFrom<SmolStr> for LiteralLongId {
    type Error = ();

    fn try_from(text: SmolStr) -> Result<Self, Self::Error> {
        Ok(Self {
            value: if let Some(num_no_prefix) = text.strip_prefix("0x") {
                BigInt::from_str_radix(num_no_prefix, 16)
            } else if let Some(num_no_prefix) = text.strip_prefix("0o") {
                BigInt::from_str_radix(num_no_prefix, 8)
            } else if let Some(num_no_prefix) = text.strip_prefix("0b") {
                BigInt::from_str_radix(num_no_prefix, 2)
            } else {
                text.parse::<BigInt>()
            }
            .map_err(|_| ())?,
        })
    }
}

use cairo_lang_utils::define_short_id;

use crate::db::SemanticGroup;

define_short_id!(LiteralId, LiteralLongId, SemanticGroup, lookup_intern_literal);
impl LiteralId {
    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        db.lookup_intern_literal(*self).value.to_string()
    }
}
