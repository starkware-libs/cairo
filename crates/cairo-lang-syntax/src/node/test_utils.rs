use cairo_lang_filesystem::db::{FilesDatabase, FilesGroup};
use cairo_lang_utils::Upcast;

use super::db::{HasGreenInterner, SyntaxDatabase};
use super::green::SyntaxInterner;

#[salsa::database(SyntaxDatabase, FilesDatabase)]
#[derive(Default)]
pub struct DatabaseForTesting {
    storage: salsa::Storage<DatabaseForTesting>,
    green_interner: SyntaxInterner,
}
impl salsa::Database for DatabaseForTesting {}
impl Upcast<dyn FilesGroup> for DatabaseForTesting {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}
impl HasGreenInterner for DatabaseForTesting {
    fn get_interner(&self) -> &SyntaxInterner {
        &self.green_interner
    }
}
