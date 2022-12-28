use cairo_filesystem::db::FilesGroup;
use cairo_utils::Upcast;

use super::green::GreenNode;
use super::ids::{GreenId, SyntaxStablePtrId};
use super::stable_ptr::SyntaxStablePtr;

// Salsa database interface.
#[salsa::query_group(SyntaxDatabase)]
pub trait SyntaxGroup: FilesGroup + Upcast<dyn FilesGroup> {
    #[salsa::interned]
    fn intern_green(&self, field: GreenNode) -> GreenId;
    #[salsa::interned]
    fn intern_stable_ptr(&self, field: SyntaxStablePtr) -> SyntaxStablePtrId;
}
