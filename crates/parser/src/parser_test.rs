use std::fs;

use filesystem::ids::FileId;
use salsa::{InternId, InternKey};
use syntax::node::db::GreenDatabase;

use crate::parser::Parser;
// use crate::printer::{print_colored, print_colored_with_missing, print_tree};

#[salsa::database(GreenDatabase)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl salsa::Database for DatabaseImpl {}

fn test_source() -> FileId {
    FileId::from_intern_id(InternId::from(100u32))
}

#[test]
fn test_simple() {
    let db_val = DatabaseImpl::default();
    let db = &db_val;

    let filename = "test_data/test_parser";
    let contents = fs::read_to_string(filename)
        .unwrap_or_else(|_| panic!("Something went wrong reading file {}", filename));
    let mut parser = Parser::from_text(db, test_source(), contents.as_str());
    let _green_root = parser.parse_compilation_unit().root;

    // TODO(yuval): print the green tree and compare to the expected string.
}
