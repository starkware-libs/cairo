use filesystem::db::{init_files_group, FilesDatabase};
use syntax::node::db::{AsSyntaxGroup, SyntaxDatabase, SyntaxGroup};

use crate::db::ParserDatabase;

// Test salsa database.
#[salsa::database(ParserDatabase, SyntaxDatabase, FilesDatabase)]
pub struct ParserDatabaseForTesting {
    storage: salsa::Storage<ParserDatabaseForTesting>,
}
impl salsa::Database for ParserDatabaseForTesting {}
impl Default for ParserDatabaseForTesting {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        res
    }
}

impl AsSyntaxGroup for ParserDatabaseForTesting {
    fn as_syntax_group(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}
