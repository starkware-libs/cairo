use std::path::PathBuf;

use db_utils::Upcast;
use diagnostics::{Diagnostics, DiagnosticsBuilder};
use filesystem::db::{init_files_group, FilesDatabase, FilesGroup};
use filesystem::ids::FileId;
use syntax::node::db::{SyntaxDatabase, SyntaxGroup};
use syntax::node::green::GreenNodeDetails;
use syntax::node::{SyntaxNode, TypedSyntaxNode};

use crate::db::ParserDatabase;
use crate::parser::Parser;
use crate::ParserDiagnostic;

/// A salsa database for parsing only.
#[salsa::database(ParserDatabase, SyntaxDatabase, FilesDatabase)]
pub struct SimpleParserDatabase {
    storage: salsa::Storage<SimpleParserDatabase>,
}
impl salsa::Database for SimpleParserDatabase {}
impl Default for SimpleParserDatabase {
    fn default() -> Self {
        let mut res = Self { storage: Default::default() };
        init_files_group(&mut res);
        res
    }
}

impl Upcast<dyn SyntaxGroup> for SimpleParserDatabase {
    fn upcast(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}

/// Reads a cairo file to the db and return the syntax_root and diagnostic of its parsing.
pub fn get_syntax_root_and_diagnostics_from_file(
    db: &SimpleParserDatabase,
    cairo_filename: &str,
) -> (SyntaxNode, Diagnostics<ParserDiagnostic>) {
    let file_id = FileId::new(db, PathBuf::from(cairo_filename));
    let contents = db.file_content(file_id).unwrap();
    get_syntax_root_and_diagnostics(db, file_id, contents.as_str())
}

/// Returns the syntax_root and diagnostic of a file in the db.
pub fn get_syntax_root_and_diagnostics(
    db: &SimpleParserDatabase,
    file_id: FileId,
    contents: &str,
) -> (SyntaxNode, Diagnostics<ParserDiagnostic>) {
    let mut diagnostics = DiagnosticsBuilder::new();
    let syntax_root = Parser::parse_file(db, &mut diagnostics, file_id, contents);
    (syntax_root.as_syntax_node(), diagnostics.build())
}

// TODO(orizi): Consider making this a method of syntax node.
/// Get all the text under a semantic node.
pub fn get_node_text(db: &dyn SyntaxGroup, syntax_node: &SyntaxNode) -> String {
    let mut buffer = String::default();
    append_text(db, syntax_node, &mut buffer);
    buffer
}

/// Helper for `get_node_text` to allocate only a single string.
fn append_text(db: &dyn SyntaxGroup, syntax_node: &SyntaxNode, buffer: &mut String) {
    let node = syntax_node.green_node(db);
    match node.details {
        GreenNodeDetails::Token(text) => {
            buffer.push_str(text.as_str());
        }
        GreenNodeDetails::Node { .. } => {
            for child in syntax_node.children(db) {
                append_text(db, &child, buffer);
            }
        }
    }
}
