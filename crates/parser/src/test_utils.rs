use filesystem::db::FilesDatabase;
use syntax::node::db::{AsSyntaxGroup, SyntaxDatabase, SyntaxGroup};

use crate::db::ParserDatabase;

// Test salsa database.
#[salsa::database(ParserDatabase, SyntaxDatabase, FilesDatabase)]
#[derive(Default)]
pub struct ParserDatabaseForTesting {
    storage: salsa::Storage<ParserDatabaseForTesting>,
}
impl salsa::Database for ParserDatabaseForTesting {}
impl AsSyntaxGroup for ParserDatabaseForTesting {
    fn as_syntax_group(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}
