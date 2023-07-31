use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_utils::Upcast;

use super::green::{GreenId, GreenNode, SyntaxInterner};
use super::stable_ptr::{SyntaxStablePtr, SyntaxStablePtrId};

// Salsa database interface.
#[salsa::query_group(SyntaxDatabase)]
pub trait SyntaxGroup: FilesGroup + Upcast<dyn FilesGroup> + HasGreenInterner {}

pub trait HasGreenInterner {
    fn get_interner(&self) -> &SyntaxInterner;
    fn intern_green(&self, node: GreenNode<'_>) -> GreenId {
        self.get_interner().intern_green(&node)
    }
    fn lookup_intern_green(&self, id: GreenId) -> GreenNode<'static> {
        self.get_interner().lookup_green(id)
    }
    fn intern_stable_ptr(&self, stable_ptr: SyntaxStablePtr<'_>) -> SyntaxStablePtrId {
        self.get_interner().intern_stable(&stable_ptr)
    }
    fn lookup_intern_stable_ptr(&self, id: SyntaxStablePtrId) -> SyntaxStablePtr<'static> {
        self.get_interner().lookup_stable(id)
    }
}
