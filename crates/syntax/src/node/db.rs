use super::{green::GreenNode, ids::GreenId};

// Salsa database interface.
#[salsa::query_group(GreenDatabase)]
pub trait GreenInterner {
    #[salsa::interned]
    fn intern_green(&self, field: GreenNode) -> GreenId;
}
