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
            value: match text.strip_prefix("0x") {
                Some(num_no_prefix) => BigInt::from_str_radix(num_no_prefix, 16),
                None => text.parse::<BigInt>(),
            }
            .map_err(|_| ())?,
        })
    }
}

use db_utils::define_short_id;

use crate::db::SemanticGroup;

define_short_id!(LiteralId, LiteralLongId, SemanticGroup, lookup_intern_literal);
impl LiteralId {
    pub fn format(&self, db: &dyn SemanticGroup) -> String {
        db.lookup_intern_literal(*self).value.to_string()
    }
}
