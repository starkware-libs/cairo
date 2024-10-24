use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe, ToMaybe};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{FileId, FileKind};
use cairo_lang_syntax::node::ast::{Expr, SyntaxFile};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::Upcast;

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
    /// Parses a file and returns its SyntaxNode.
    fn file_syntax(&self, file_id: FileId) -> Maybe<SyntaxNode>;
    /// Parses a file and returns its AST as a root SyntaxFile.
    fn file_module_syntax(&self, file_id: FileId) -> Maybe<SyntaxFile>;
    /// Parses a file and returns its AST as an expression.
    fn file_expr_syntax(&self, file_id: FileId) -> Maybe<Expr>;
    /// Returns the parser diagnostics for this file.
    fn file_syntax_diagnostics(&self, file_id: FileId) -> Diagnostics<ParserDiagnostic>;
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SyntaxData {
    diagnostics: Diagnostics<ParserDiagnostic>,
    syntax: Maybe<SyntaxNode>,
}

pub fn priv_file_syntax_data(db: &dyn ParserGroup, file_id: FileId) -> SyntaxData {
    let mut diagnostics = DiagnosticsBuilder::default();
    let syntax = db.file_content(file_id).to_maybe().map(|s| match file_id.kind(db.upcast()) {
        FileKind::Module => {
            Parser::parse_file(db.upcast(), &mut diagnostics, file_id, &s).as_syntax_node()
        }
        FileKind::Expr => {
            Parser::parse_file_expr(db.upcast(), &mut diagnostics, file_id, &s).as_syntax_node()
        }
    });
    SyntaxData { diagnostics: diagnostics.build(), syntax }
}

pub fn file_syntax(db: &dyn ParserGroup, file_id: FileId) -> Maybe<SyntaxNode> {
    db.priv_file_syntax_data(file_id).syntax
}

pub fn file_module_syntax(db: &dyn ParserGroup, file_id: FileId) -> Maybe<SyntaxFile> {
    assert_eq!(file_id.kind(db.upcast()), FileKind::Module, "file_id must be a module");
    Ok(SyntaxFile::from_syntax_node(db.upcast(), db.file_syntax(file_id)?))
}

pub fn file_expr_syntax(db: &dyn ParserGroup, file_id: FileId) -> Maybe<Expr> {
    assert_eq!(file_id.kind(db.upcast()), FileKind::Expr, "file_id must be a module");
    Ok(Expr::from_syntax_node(db.upcast(), db.file_syntax(file_id)?))
}

pub fn file_syntax_diagnostics(
    db: &dyn ParserGroup,
    file_id: FileId,
) -> Diagnostics<ParserDiagnostic> {
    db.priv_file_syntax_data(file_id).diagnostics
}
