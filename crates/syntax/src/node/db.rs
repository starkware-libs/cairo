use super::green::GreenNode;
use super::ids::GreenId;

// Salsa database interface.
#[salsa::query_group(GreenDatabase)]
pub trait SyntaxGroup {
    #[salsa::interned]
    fn intern_green(&self, field: GreenNode) -> GreenId;
}

pub trait AsSyntaxGroup {
    fn as_syntax_group(&self) -> &(dyn SyntaxGroup + 'static);
}
