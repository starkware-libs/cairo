use crate::ids::*;

// Salsa database interface.
// See ids.rs for further details.
#[salsa::query_group(DefsDatabase)]
pub trait DefsGroup {
    #[salsa::interned]
    fn intern_struct(&self, id: StructLongId) -> StructId;
}
