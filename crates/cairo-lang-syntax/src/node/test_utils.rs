use std::sync::RwLock;

use cairo_lang_filesystem::db::{FilesDatabase, FilesGroup};
use cairo_lang_utils::Upcast;

use super::db::{HasGreenInterner, SyntaxDatabase};
use super::green::GreenInterner;

#[salsa::database(SyntaxDatabase, FilesDatabase)]
#[derive(Default)]
pub struct DatabaseForTesting {
    storage: salsa::Storage<DatabaseForTesting>,
    green_interner: RwLock<GreenInterner>,
}
impl salsa::Database for DatabaseForTesting {}
impl Upcast<dyn FilesGroup> for DatabaseForTesting {
    fn upcast(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}
impl HasGreenInterner for DatabaseForTesting {
    fn get_interner(&self) -> &RwLock<GreenInterner> {
        &self.green_interner
    }
}
