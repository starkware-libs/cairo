use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe, ToMaybe};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{FileId, FileKind};
use cairo_lang_syntax::node::ast::{Expr, StatementList, SyntaxFile};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};

use crate::diagnostic::ParserDiagnostic;
use crate::parser::Parser;

#[cfg(test)]
#[path = "db_test.rs"]
mod db_test;

// Salsa database interface.
#[cairo_lang_proc_macros::query_group(ParserDatabase)]
pub trait ParserGroup: SyntaxGroup + FilesGroup {
    /// Should only be used internally.
    /// Parses a file and returns the result and the generated [ParserDiagnostic].
    fn priv_file_syntax_data<'a>(&'a self, file_id: FileId<'a>) -> SyntaxData<'a>;
    /// Parses a file and returns its SyntaxNode.
    fn file_syntax<'a>(&'a self, file_id: FileId<'a>) -> Maybe<SyntaxNode<'a>>;
    /// Parses a file and returns its AST as a root SyntaxFile.
    fn file_module_syntax<'a>(&'a self, file_id: FileId<'a>) -> Maybe<SyntaxFile<'a>>;
    /// Parses a file and returns its AST as an expression. Only used for inline macros expanded
    /// code.
    fn file_expr_syntax<'a>(&'a self, file_id: FileId<'a>) -> Maybe<Expr<'a>>;
    /// Parses a file and returns its AST as a list of statements. Only used for inline macros
    /// expanded code.
    fn file_statement_list_syntax<'a>(&'a self, file_id: FileId<'a>) -> Maybe<StatementList<'a>>;
    /// Returns the parser diagnostics for this file.
    fn file_syntax_diagnostics<'a>(
        &'a self,
        file_id: FileId<'a>,
    ) -> Diagnostics<'a, ParserDiagnostic<'a>>;
}

#[derive(Clone, PartialEq, Eq, Debug, salsa::Update)]
pub struct SyntaxData<'a> {
    diagnostics: Diagnostics<'a, ParserDiagnostic<'a>>,
    syntax: Maybe<SyntaxNode<'a>>,
}

pub fn priv_file_syntax_data<'a>(db: &'a dyn ParserGroup, file_id: FileId<'a>) -> SyntaxData<'a> {
    let mut diagnostics = DiagnosticsBuilder::default();
    let syntax = db.file_content(file_id).to_maybe().map(|s| match file_id.kind(db) {
        FileKind::Module => {
            Parser::parse_file(db, &mut diagnostics, file_id, s.long(db).as_ref()).as_syntax_node()
        }
        FileKind::Expr => {
            Parser::parse_file_expr(db, &mut diagnostics, file_id, s.long(db).as_ref())
                .as_syntax_node()
        }
        FileKind::StatementList => {
            Parser::parse_file_statement_list(db, &mut diagnostics, file_id, s.long(db))
                .as_syntax_node()
        }
    });
    SyntaxData { diagnostics: diagnostics.build(), syntax }
}

pub fn file_syntax<'a>(db: &'a dyn ParserGroup, file_id: FileId<'a>) -> Maybe<SyntaxNode<'a>> {
    db.priv_file_syntax_data(file_id).syntax
}

pub fn file_module_syntax<'a>(
    db: &'a dyn ParserGroup,
    file_id: FileId<'a>,
) -> Maybe<SyntaxFile<'a>> {
    assert_eq!(file_id.kind(db), FileKind::Module, "file_id must be a module");
    Ok(SyntaxFile::from_syntax_node(db, db.file_syntax(file_id)?))
}

pub fn file_expr_syntax<'a>(db: &'a dyn ParserGroup, file_id: FileId<'a>) -> Maybe<Expr<'a>> {
    assert_eq!(file_id.kind(db), FileKind::Expr, "file_id must be an expr");
    Ok(Expr::from_syntax_node(db, db.file_syntax(file_id)?))
}

pub fn file_statement_list_syntax<'a>(
    db: &'a dyn ParserGroup,
    file_id: FileId<'a>,
) -> Maybe<StatementList<'a>> {
    assert_eq!(file_id.kind(db), FileKind::StatementList, "file_id must be a for a statement list");
    Ok(StatementList::from_syntax_node(db, db.file_syntax(file_id)?))
}

pub fn file_syntax_diagnostics<'a>(
    db: &'a dyn ParserGroup,
    file_id: FileId<'a>,
) -> Diagnostics<'a, ParserDiagnostic<'a>> {
    db.priv_file_syntax_data(file_id).diagnostics
}
