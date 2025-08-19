use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe, ToMaybe};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{FileId, FileKind};
use cairo_lang_syntax::node::ast::{Expr, StatementList, SyntaxFile};
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};

use crate::diagnostic::ParserDiagnostic;
use crate::parser::Parser;

#[cfg(test)]
#[path = "db_test.rs"]
mod db_test;

// TODO(orizi): Remove `query_group` macro and have this implemented for `salsa::Database` directly.
/// Interface of the parser database.
#[cairo_lang_proc_macros::query_group(ParserDatabase)]
pub trait ParserGroup: FilesGroup {
    /// Parses a file and returns its AST as a root SyntaxFile.
    #[salsa::transparent]
    fn file_module_syntax<'db>(&'db self, file_id: FileId<'db>) -> Maybe<SyntaxFile<'db>>;
    /// Parses a file and returns its AST as an expression. Only used for inline macros expanded
    /// code.
    #[salsa::transparent]
    fn file_expr_syntax<'db>(&'db self, file_id: FileId<'db>) -> Maybe<Expr<'db>>;
    /// Parses a file and returns its AST as a list of statements. Only used for inline macros
    /// expanded code.
    #[salsa::transparent]
    fn file_statement_list_syntax<'db>(
        &'db self,
        file_id: FileId<'db>,
    ) -> Maybe<StatementList<'db>>;
    /// Returns the parser diagnostics for this file.
    #[salsa::transparent]
    fn file_syntax_diagnostics<'db>(
        &'db self,
        file_id: FileId<'db>,
    ) -> Diagnostics<'db, ParserDiagnostic<'db>>;
}

#[salsa::tracked]
struct SyntaxData<'db> {
    diagnostics: Diagnostics<'db, ParserDiagnostic<'db>>,
    syntax: Maybe<SyntaxNode<'db>>,
}

/// Parses a file and returns the result and the generated [ParserDiagnostic].
#[salsa::tracked(returns(ref))]
fn file_syntax_data<'db>(db: &'db dyn ParserGroup, file_id: FileId<'db>) -> SyntaxData<'db> {
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
    SyntaxData::new(db, diagnostics.build(), syntax)
}

/// Parses a file and returns its SyntaxNode.
fn file_syntax<'db>(db: &'db dyn ParserGroup, file_id: FileId<'db>) -> Maybe<SyntaxNode<'db>> {
    file_syntax_data(db, file_id).syntax(db)
}

fn file_module_syntax<'db>(
    db: &'db dyn ParserGroup,
    file_id: FileId<'db>,
) -> Maybe<SyntaxFile<'db>> {
    assert_eq!(file_id.kind(db), FileKind::Module, "file_id must be a module");
    Ok(SyntaxFile::from_syntax_node(db, file_syntax(db, file_id)?))
}

fn file_expr_syntax<'db>(db: &'db dyn ParserGroup, file_id: FileId<'db>) -> Maybe<Expr<'db>> {
    assert_eq!(file_id.kind(db), FileKind::Expr, "file_id must be an expr");
    Ok(Expr::from_syntax_node(db, file_syntax(db, file_id)?))
}

fn file_statement_list_syntax<'db>(
    db: &'db dyn ParserGroup,
    file_id: FileId<'db>,
) -> Maybe<StatementList<'db>> {
    assert_eq!(file_id.kind(db), FileKind::StatementList, "file_id must be a for a statement list");
    Ok(StatementList::from_syntax_node(db, file_syntax(db, file_id)?))
}

fn file_syntax_diagnostics<'db>(
    db: &'db dyn ParserGroup,
    file_id: FileId<'db>,
) -> Diagnostics<'db, ParserDiagnostic<'db>> {
    file_syntax_data(db, file_id).diagnostics(db).clone()
}
