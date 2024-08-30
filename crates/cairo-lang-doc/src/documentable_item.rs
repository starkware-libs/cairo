use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{LanguageElementId, LookupItemId, MemberId, VariantId};
use cairo_lang_filesystem::ids::CrateId;

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub enum DocumentableItemId {
    Crate(CrateId),
    Item(DocumentableModuleItemId),
}

impl From<CrateId> for DocumentableItemId {
    fn from(value: CrateId) -> Self {
        DocumentableItemId::Crate(value)
    }
}

impl From<DocumentableModuleItemId> for DocumentableItemId {
    fn from(value: DocumentableModuleItemId) -> Self {
        DocumentableItemId::Item(value)
    }
}

impl From<LookupItemId> for DocumentableItemId {
    fn from(value: LookupItemId) -> Self {
        DocumentableItemId::from(DocumentableModuleItemId::from(value))
    }
}

impl From<MemberId> for DocumentableItemId {
    fn from(value: MemberId) -> Self {
        DocumentableItemId::from(DocumentableModuleItemId::from(value))
    }
}
impl From<VariantId> for DocumentableItemId {
    fn from(value: VariantId) -> Self {
        DocumentableItemId::from(DocumentableModuleItemId::from(value))
    }
}

/// Item which documentation can be fetched from source code.
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub enum DocumentableModuleItemId {
    LookupItem(LookupItemId),
    Member(MemberId),
    Variant(VariantId),
}

impl DocumentableModuleItemId {
    pub fn stable_location(&self, db: &dyn DefsGroup) -> StableLocation {
        match self {
            DocumentableModuleItemId::LookupItem(lookup_item_id) => {
                lookup_item_id.stable_location(db)
            }
            DocumentableModuleItemId::Member(member_id) => member_id.stable_location(db),
            DocumentableModuleItemId::Variant(variant_id) => variant_id.stable_location(db),
        }
    }
}

impl From<LookupItemId> for DocumentableModuleItemId {
    fn from(value: LookupItemId) -> Self {
        DocumentableModuleItemId::LookupItem(value)
    }
}

impl From<MemberId> for DocumentableModuleItemId {
    fn from(value: MemberId) -> Self {
        DocumentableModuleItemId::Member(value)
    }
}
impl From<VariantId> for DocumentableModuleItemId {
    fn from(value: VariantId) -> Self {
        DocumentableModuleItemId::Variant(value)
    }
}
