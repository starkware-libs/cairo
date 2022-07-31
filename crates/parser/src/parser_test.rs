#![allow(unused_imports)]

use std::fs;

use filesystem::ids::FileId;
use salsa::{InternId, InternKey};
use syntax::node::db::GreenDatabase;

use crate::parser::Parser;
use crate::printer::{print_colored, print_colored_with_missing, print_tree};

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

    let filename = "/home/yuval/workspace/cairo2/crates/parser/test_data/test_printer";
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    print!("{}", contents);
    let mut parser = Parser::from_text(db, test_source(), contents.as_str());
    let green_root = parser.parse_compilation_unit().root;

    println!("Colored print:\n");
    print_colored(green_root, db);

    println!("Colored print with missing:\n");
    print_colored_with_missing(green_root, db);

    println!("Tree print:\n");
    print_tree(green_root, db, true);
}
