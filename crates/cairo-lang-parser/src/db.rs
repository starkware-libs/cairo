use std::sync::Arc;

use cairo_lang_diagnostics::{Diagnostics, DiagnosticsBuilder, Maybe, ToMaybe};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{FileId, FileKind};
use cairo_lang_syntax::node::ast::{Expr, StatementList, SyntaxFile};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::ids::GreenId;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::Upcast;

use crate::diagnostic::ParserDiagnostic;
use crate::parser::Parser;

#[cfg(test)]
#[path = "db_test.rs"]
mod db_test;

// Salsa database interface.
#[salsa::query_group(ParserDatabase)]
pub trait ParserGroup:
    SyntaxGroup + Upcast<dyn SyntaxGroup> + FilesGroup + Upcast<dyn FilesGroup>
{
    /// Should only be used internally.
    /// Parses a file and returns the result and the generated [ParserDiagnostic].
    fn priv_file_syntax_data(&self, file_id: FileId) -> SyntaxData;
    /// Parses a file and returns its SyntaxNode.
    fn file_syntax(&self, file_id: FileId) -> Maybe<SyntaxNode>;
    /// Parses a file and returns its GreeIn list.
    fn file_tokens(&self, file_id: FileId) -> Maybe<Arc<[GreenId]>>;
    /// Parses a file and returns its AST as a root SyntaxFile.
    fn file_module_syntax(&self, file_id: FileId) -> Maybe<SyntaxFile>;
    /// Parses a file and returns its AST as an expression. Only used for inline macros expanded
    /// code.
    fn file_expr_syntax(&self, file_id: FileId) -> Maybe<Expr>;
    /// Parses a file and returns its AST as a list of statements. Only used for inline macros
    /// expanded code.
    fn file_statement_list_syntax(&self, file_id: FileId) -> Maybe<StatementList>;
    /// Returns the parser diagnostics for this file.
    fn file_syntax_diagnostics(&self, file_id: FileId) -> Diagnostics<ParserDiagnostic>;
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct SyntaxData {
    diagnostics: Diagnostics<ParserDiagnostic>,
    syntax: Maybe<(Arc<[GreenId]>, SyntaxNode)>,
}

pub fn priv_file_syntax_data(db: &dyn ParserGroup, file_id: FileId) -> SyntaxData {
    let mut diagnostics = DiagnosticsBuilder::default();
    let syntax = db.file_content(file_id).to_maybe().map(|s| match file_id.kind(db) {
        FileKind::Module => {
            let (terminals, node) = Parser::parse_file(db, &mut diagnostics, file_id, &s);
            (terminals.into(), node.as_syntax_node())
        }
        FileKind::Expr => {
            let (terminals, node) = Parser::parse_file_expr(db, &mut diagnostics, file_id, &s);
            (terminals.into(), node.as_syntax_node())
        }
        FileKind::StatementList => {
            let (terminals, node) =
                Parser::parse_file_statement_list(db, &mut diagnostics, file_id, &s);
            (terminals.into(), node.as_syntax_node())
        }
    });
    SyntaxData { diagnostics: diagnostics.build(), syntax }
}

pub fn file_syntax(db: &dyn ParserGroup, file_id: FileId) -> Maybe<SyntaxNode> {
    Ok(db.priv_file_syntax_data(file_id).syntax?.1)
}

pub fn file_tokens(db: &dyn ParserGroup, file_id: FileId) -> Maybe<Arc<[GreenId]>> {
    Ok(db.priv_file_syntax_data(file_id).syntax?.0)
}

pub fn file_module_syntax(db: &dyn ParserGroup, file_id: FileId) -> Maybe<SyntaxFile> {
    assert_eq!(file_id.kind(db), FileKind::Module, "file_id must be a module");
    Ok(SyntaxFile::from_syntax_node(db, db.file_syntax(file_id)?))
}

pub fn file_expr_syntax(db: &dyn ParserGroup, file_id: FileId) -> Maybe<Expr> {
    assert_eq!(file_id.kind(db), FileKind::Expr, "file_id must be an expr");
    Ok(Expr::from_syntax_node(db, db.file_syntax(file_id)?))
}

pub fn file_statement_list_syntax(db: &dyn ParserGroup, file_id: FileId) -> Maybe<StatementList> {
    assert_eq!(file_id.kind(db), FileKind::StatementList, "file_id must be a for a statement list");
    Ok(StatementList::from_syntax_node(db, db.file_syntax(file_id)?))
}

pub fn file_syntax_diagnostics(
    db: &dyn ParserGroup,
    file_id: FileId,
) -> Diagnostics<ParserDiagnostic> {
    db.priv_file_syntax_data(file_id).diagnostics
}

pub trait SyntaxNodeExt {
    /// Gets all the leaves of the SyntaxTree, where the self node is the root of a tree.
    fn tokens<'a>(&'a self, db: &'a dyn ParserGroup) -> impl IntoIterator<Item = GreenId> + 'a;
}

impl SyntaxNodeExt for SyntaxNode {
    fn tokens<'a>(&'a self, db: &'a dyn ParserGroup) -> impl IntoIterator<Item = GreenId> + 'a {
        let span = self.span(db);

        let mut offset = 0;
        let mut width = 0;
        let offset = &mut offset;
        let width = &mut width;
        let tokens = db.file_tokens(self.stable_ptr(db).file_id(db)).unwrap();
        tokens
            .into_iter()
            .skip_while(move |green| {
                let skip = *offset != span.start.as_u32();
                *offset += green.width(db).as_u32();

                skip
            })
            .take_while(move |green| {
                let take = *width != span.width().as_u32();
                *width += green.width(db).as_u32();

                take
            })
            .copied()
            .collect::<Vec<_>>()
    }
}
