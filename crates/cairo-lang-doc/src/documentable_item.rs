use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    LanguageElementId, LookupItemId, MemberId, NamedLanguageElementId, VariantId,
};
use cairo_lang_filesystem::ids::CrateId;
use smol_str::SmolStr;

/// Item whose documentation can be fetched from source code.
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub enum DocumentableItemId {
    Crate(CrateId),
    LookupItem(LookupItemId),
    Member(MemberId),
    Variant(VariantId),
}

impl DocumentableItemId {
    pub fn stable_location(&self, db: &dyn DefsGroup) -> Option<StableLocation> {
        match self {
            DocumentableItemId::Crate(_) => None,
            DocumentableItemId::LookupItem(lookup_item_id) => {
                Some(lookup_item_id.stable_location(db))
            }
            DocumentableItemId::Member(member_id) => Some(member_id.stable_location(db)),
            DocumentableItemId::Variant(variant_id) => Some(variant_id.stable_location(db)),
        }
    }

    /// Gets the name of the item.
    pub fn name(&self, db: &dyn DefsGroup) -> SmolStr {
        match self {
            DocumentableItemId::LookupItem(LookupItemId::ModuleItem(id)) => id.name(db),
            DocumentableItemId::LookupItem(LookupItemId::ImplItem(id)) => id.name(db),
            DocumentableItemId::LookupItem(LookupItemId::TraitItem(id)) => id.name(db),
            DocumentableItemId::Crate(id) => id.name(db.upcast()),
            DocumentableItemId::Member(id) => id.name(db),
            DocumentableItemId::Variant(id) => id.name(db),
        }
    }
}

impl From<CrateId> for DocumentableItemId {
    fn from(value: CrateId) -> Self {
        DocumentableItemId::Crate(value)
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
