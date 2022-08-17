use std::sync::Arc;

use db_utils::Upcast;
use filesystem::db::FilesGroup;
use filesystem::ids::FileId;
use syntax::node::ast::SyntaxFile;
use syntax::node::db::GreenInterner;

use crate::parser::Parser;

#[cfg(test)]
#[path = "db_test.rs"]
mod db_test;

// Salsa database interface.
#[salsa::query_group(ParserDatabase)]
pub trait ParserGroup: GreenInterner + Upcast<dyn GreenInterner> + FilesGroup {
    fn file_syntax(&self, file_id: FileId) -> Arc<SyntaxFile>;
}

pub fn file_syntax(db: &dyn ParserGroup, file_id: FileId) -> Arc<SyntaxFile> {
    let s = db.file_content(file_id).unwrap().clone();
    let mut parser = Parser::from_text(db.upcast(), file_id, s.as_str());
    parser.parse_file()
}
