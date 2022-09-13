use std::sync::Arc;

use diagnostics::Diagnostics;
use filesystem::db::FilesGroup;
use filesystem::ids::FileId;
use syntax::node::ast::SyntaxFile;
use syntax::node::db::{AsSyntaxGroup, SyntaxGroup};

use crate::diagnostic::ParserDiagnostic;
use crate::parser::Parser;

#[cfg(test)]
#[path = "db_test.rs"]
mod db_test;

// Salsa database interface.
#[salsa::query_group(ParserDatabase)]
pub trait ParserGroup: SyntaxGroup + AsSyntaxGroup + FilesGroup {
    /// Should only be used internally.
    /// Parses a file and returns the result and the generated [ParserDiagnostic].
    fn priv_file_syntax_data(&self, file_id: FileId) -> SyntaxData;
    /// Parses a file and returns its AST.
    fn file_syntax(&self, file_id: FileId) -> Option<Arc<SyntaxFile>>;
    /// Returns the parser diagnostics for this file.
    fn file_syntax_diagnostics(&self, file_id: FileId) -> Arc<Diagnostics<ParserDiagnostic>>;
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SyntaxData {
    diagnostics: Arc<Diagnostics<ParserDiagnostic>>,
    syntax: Option<Arc<SyntaxFile>>,
}

pub fn priv_file_syntax_data(db: &dyn ParserGroup, file_id: FileId) -> SyntaxData {
    let mut diagnostics = Diagnostics::default();
    let syntax = db.file_content(file_id).map(|s| {
        Arc::new(Parser::parse_file(db.as_syntax_group(), &mut diagnostics, file_id, s.as_str()))
    });
    SyntaxData { diagnostics: Arc::new(diagnostics), syntax }
}

pub fn file_syntax(db: &dyn ParserGroup, file_id: FileId) -> Option<Arc<SyntaxFile>> {
    db.priv_file_syntax_data(file_id).syntax
}

pub fn file_syntax_diagnostics(
    db: &dyn ParserGroup,
    file_id: FileId,
) -> Arc<Diagnostics<ParserDiagnostic>> {
    db.priv_file_syntax_data(file_id).diagnostics
}
