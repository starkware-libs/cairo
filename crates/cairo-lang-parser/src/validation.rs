//! This module implements syntax validation that the parser doesn't handle.
//!
//! A failed validation emits a diagnostic.

use cairo_lang_diagnostics::{DiagnosticsBuilder, Maybe};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, SyntaxNode, Terminal, TypedSyntaxNode};
use num_bigint::BigInt;
use num_traits::Num;
use unescaper::unescape;

use crate::diagnostic::ParserDiagnosticKind;
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
            SyntaxKind::TerminalNumber => {
                let node = ast::TerminalNumber::from_syntax_node(db, node);
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

/// Validate that the numeric literal is valid, after it is consumed by the parser.
///
/// Cairo parser tries to consume even not proper tokens in order to support code editions in IDEs.
/// This means that it omits some crucial details in the literals that make the code uncompilable.
/// This function validates that the literal:
/// 1. Is parseable according to its radix.
/// 2. Has properly formatted suffix.
fn validate_literal_number(
    node: ast::TerminalNumber,
    db: &dyn SyntaxGroup,
    diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
    file_id: FileId,
) -> Maybe<()> {
    let mut result = Ok(());

    let text = node.text(db);

    let (text, ty) = match text.split_once('_') {
        Some((text, ty)) => (text, Some(ty)),
        None => (text.as_str(), None),
    };

    // Verify number value is parseable.
    {
        let (text, radix) = if let Some(num_no_prefix) = text.strip_prefix("0x") {
            (num_no_prefix, 16)
        } else if let Some(num_no_prefix) = text.strip_prefix("0o") {
            (num_no_prefix, 8)
        } else if let Some(num_no_prefix) = text.strip_prefix("0b") {
            (num_no_prefix, 2)
        } else {
            (text, 10)
        };

        if BigInt::from_str_radix(text, radix).is_err() {
            result = Err(diagnostics.add(ParserDiagnostic {
                file_id,
                span: node.as_syntax_node().span(db),
                kind: ParserDiagnosticKind::InvalidNumericLiteralValue,
            }));
        }
    }

    // Verify suffix.
    if let Some(ty) = ty {
        if ty.is_empty() {
            result = Err(diagnostics.add(ParserDiagnostic {
                file_id,
                span: node.as_syntax_node().span(db).after(),
                kind: ParserDiagnosticKind::MissingLiteralSuffix,
            }));
        }
    }

    result
}

/// Validate that the short string literal is valid, after it is consumed by the parser.
///
/// Cairo parser tries to consume even not proper tokens in order to support code editions in IDEs.
/// This means that it omits some crucial details in the literals that make the code uncompilable.
/// This function validates that the literal:
/// 1. Has single quotes on both sides (parser accepts unterminated literals).
/// 2. Has all escape sequences valid.
/// 3. Is entirely ASCII.
fn validate_short_string(
    node: ast::TerminalShortString,
    db: &dyn SyntaxGroup,
    diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
    file_id: FileId,
) -> Maybe<()> {
    let mut result = Ok(());

    let text = node.text(db);
    let mut text = text.as_str();

    if text.starts_with('\'') {
        (_, text) = text.split_once('\'').unwrap();
    } else {
        // NOTE: This is a very paranoid case, but let's try to recover anyway here instead of
        //   panicking.
        result = Err(diagnostics.add(ParserDiagnostic {
            file_id,
            span: node.as_syntax_node().span(db),
            kind: ParserDiagnosticKind::UnterminatedString,
        }));
    }

    let (body, _suffix) = match text.rsplit_once('\'') {
        Some((body, suffix)) => (body, (!suffix.is_empty()).then_some(suffix)),
        None => {
            result = Err(diagnostics.add(ParserDiagnostic {
                file_id,
                span: node.as_syntax_node().span(db),
                kind: ParserDiagnosticKind::UnterminatedString,
            }));

            (text, None)
        }
    };

    let body = match unescape(body) {
        Ok(body) => body,
        Err(_) => {
            // TODO(mkaput): Try to always provide full position for entire escape sequence.
            result = Err(diagnostics.add(ParserDiagnostic {
                file_id,
                span: node.as_syntax_node().span(db),
                kind: ParserDiagnosticKind::IllegalStringEscaping,
            }));

            String::new()
        }
    };

    if !body.is_ascii() {
        // TODO(mkaput): Try to always provide position of culprit character/escape sequence.
        result = Err(diagnostics.add(ParserDiagnostic {
            file_id,
            span: node.as_syntax_node().span(db),
            kind: ParserDiagnosticKind::ShortStringMustBeAscii,
        }));
    }

    result
}
