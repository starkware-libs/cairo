//! This module implements syntax validation that the parser doesn't handle.
//!
//! A failed validation emits a diagnostic.

use cairo_lang_diagnostics::{DiagnosticsBuilder, Maybe};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, SyntaxNode, TypedSyntaxNode};

use crate::ParserDiagnostic;

/// Validate syntax nodes for aspects that are not handled by the parser.
///
/// This includes things like:
/// 1. Validating string escape sequences.
/// 2. Validating numeric literals that they indeed represent valid values (untyped).
///
/// Not all usage places of the parser require this pass. Primary example is Cairo formatter - it
/// should work even if strings contain invalid escape sequences in strings.
pub fn validate(
    root: SyntaxNode,
    db: &dyn SyntaxGroup,
    diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
    file_id: FileId,
) -> Maybe<()> {
    root.descendants(db).fold(Ok(()), |result, node| {
        result.and(match node.kind(db) {
            SyntaxKind::TerminalLiteralNumber => {
                let node = ast::TerminalLiteralNumber::from_syntax_node(db, node);
                validate_literal_number(node, db, diagnostics, file_id)
            }

            SyntaxKind::TerminalShortString => {
                let node = ast::TerminalShortString::from_syntax_node(db, node);
                validate_short_string(node, db, diagnostics, file_id)
            }

            _ => Ok(()),
        })
    })
}

fn validate_literal_number(
    _node: ast::TerminalLiteralNumber,
    _db: &dyn SyntaxGroup,
    _diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
    _file_id: FileId,
) -> Maybe<()> {
    // TODO
    Ok(())
}

fn validate_short_string(
    _node: ast::TerminalShortString,
    _db: &dyn SyntaxGroup,
    _diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
    _file_id: FileId,
) -> Maybe<()> {
    // TODO
    Ok(())
}
