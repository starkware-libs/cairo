use std::sync::Arc;

use db_utils::Upcast;
use diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe, ToMaybe};
use filesystem::db::FilesGroup;
use filesystem::ids::FileId;
use syntax::node::ast::SyntaxFile;
use syntax::node::db::SyntaxGroup;

use crate::diagnostic::ParserDiagnostic;
use crate::parser::Parser;

#[cfg(test)]
#[path = "db_test.rs"]
mod db_test;

// Salsa database interface.
#[salsa::query_group(ParserDatabase)]
pub trait ParserGroup: SyntaxGroup + Upcast<dyn SyntaxGroup> + FilesGroup {
    /// Should only be used internally.
    /// Parses a file and returns the result and the generated [ParserDiagnostic].
    fn priv_file_syntax_data(&self, file_id: FileId) -> SyntaxData;
    /// Parses a file and returns its AST.
    fn file_syntax(&self, file_id: FileId) -> Maybe<Arc<SyntaxFile>>;
    /// Returns the parser diagnostics for this file.
    fn file_syntax_diagnostics(&self, file_id: FileId) -> Diagnostics<ParserDiagnostic>;
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SyntaxData {
    diagnostics: Diagnostics<ParserDiagnostic>,
    syntax: Maybe<Arc<SyntaxFile>>,
}

pub fn priv_file_syntax_data(db: &dyn ParserGroup, file_id: FileId) -> SyntaxData {
    let mut diagnostics = DiagnosticsBuilder::default();
    let syntax = db
        .file_content(file_id)
        .to_maybe()
        .map(|s| Arc::new(Parser::parse_file(db.upcast(), &mut diagnostics, file_id, s.as_str())));
    SyntaxData { diagnostics: diagnostics.build(), syntax }
}

pub fn file_syntax(db: &dyn ParserGroup, file_id: FileId) -> Maybe<Arc<SyntaxFile>> {
    db.priv_file_syntax_data(file_id).syntax
}

pub fn file_syntax_diagnostics(
    db: &dyn ParserGroup,
    file_id: FileId,
) -> Diagnostics<ParserDiagnostic> {
    db.priv_file_syntax_data(file_id).diagnostics
}
