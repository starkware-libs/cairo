use super::green::GreenNode;
use super::ids::{GreenId, SyntaxStablePtrId};
use super::stable_ptr::SyntaxStablePtr;

// Salsa database interface.
#[salsa::query_group(GreenDatabase)]
pub trait GreenInterner {
    #[salsa::interned]
    fn intern_green(&self, field: GreenNode) -> GreenId;
    #[salsa::interned]
    fn intern_stable_ptr(&self, field: SyntaxStablePtr) -> SyntaxStablePtrId;
}

pub trait AsGreenInterner {
    fn as_green_interner(&self) -> &(dyn GreenInterner + 'static);
}
