use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    LanguageElementId, LookupItemId, MemberId, NamedLanguageElementId, VariantId,
};
use cairo_lang_filesystem::ids::{CrateId, SmolStrId};
use salsa::Database;

/// Item whose documentation can be fetched from source code.
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, salsa::Update)]
pub enum DocumentableItemId<'db> {
    Crate(CrateId<'db>),
    LookupItem(LookupItemId<'db>),
    Member(MemberId<'db>),
    Variant(VariantId<'db>),
}

impl<'db> DocumentableItemId<'db> {
    pub fn stable_location(&self, db: &'db dyn Database) -> Option<StableLocation<'db>> {
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
    pub fn name(&self, db: &'db dyn Database) -> SmolStrId<'db> {
        match self {
            DocumentableItemId::LookupItem(LookupItemId::ModuleItem(id)) => id.name(db),
            DocumentableItemId::LookupItem(LookupItemId::ImplItem(id)) => id.name(db),
            DocumentableItemId::LookupItem(LookupItemId::TraitItem(id)) => id.name(db),
            DocumentableItemId::Crate(id) => id.long(db).name(),
            DocumentableItemId::Member(id) => id.name(db),
            DocumentableItemId::Variant(id) => id.name(db),
        }
    }
}

impl<'db> From<CrateId<'db>> for DocumentableItemId<'db> {
    fn from(value: CrateId<'db>) -> Self {
        DocumentableItemId::Crate(value)
    }
}

impl<'db> From<LookupItemId<'db>> for DocumentableItemId<'db> {
    fn from(value: LookupItemId<'db>) -> Self {
        DocumentableItemId::LookupItem(value)
    }
}

impl<'db> From<MemberId<'db>> for DocumentableItemId<'db> {
    fn from(value: MemberId<'db>) -> Self {
        DocumentableItemId::Member(value)
    }
}
impl<'db> From<VariantId<'db>> for DocumentableItemId<'db> {
    fn from(value: VariantId<'db>) -> Self {
        DocumentableItemId::Variant(value)
    }
}
