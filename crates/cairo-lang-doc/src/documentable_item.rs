use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{LanguageElementId, LookupItemId, MemberId, VariantId};

/// Item which documentation can be fetched from source code.
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub enum DocumentableItemId {
    LookupItem(LookupItemId),
    Member(MemberId),
    Variant(VariantId),
}

impl DocumentableItemId {
    pub fn stable_location(&self, db: &dyn DefsGroup) -> StableLocation {
        match self {
            DocumentableItemId::LookupItem(lookup_item_id) => lookup_item_id.stable_location(db),
            DocumentableItemId::Member(member_id) => member_id.stable_location(db),
            DocumentableItemId::Variant(variant_id) => variant_id.stable_location(db),
        }
    }
}

impl From<LookupItemId> for DocumentableItemId {
    fn from(value: LookupItemId) -> Self {
        DocumentableItemId::LookupItem(value)
    }
}

impl From<MemberId> for DocumentableItemId {
    fn from(value: MemberId) -> Self {
        DocumentableItemId::Member(value)
    }
}
impl From<VariantId> for DocumentableItemId {
    fn from(value: VariantId) -> Self {
        DocumentableItemId::Variant(value)
    }
}
