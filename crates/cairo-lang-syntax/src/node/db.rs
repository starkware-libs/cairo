use std::sync::RwLock;

use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_utils::Upcast;

use super::green::{GreenInterner, GreenNode};
use super::ids::{GreenId, SyntaxStablePtrId};
use super::stable_ptr::SyntaxStablePtr;

// Salsa database interface.
#[salsa::query_group(SyntaxDatabase)]
pub trait SyntaxGroup: FilesGroup + Upcast<dyn FilesGroup> + HasGreenInterner {
    #[salsa::interned]
    fn intern_stable_ptr(&self, field: SyntaxStablePtr) -> SyntaxStablePtrId;
}

pub trait HasGreenInterner {
    fn get_interner(&self) -> &RwLock<GreenInterner>;
    fn intern_green(&self, node: GreenNode) -> GreenId {
        self.get_interner().write().unwrap().intern(&node)
    }
    fn lookup_intern_green(&self, id: GreenId) -> GreenNode {
        self.get_interner().read().unwrap().lookup(id)
    }
}
