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

use crate::diagnostic::{ParserDiagnosticKind, ParserDiagnosticKindMissing};
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
                validate_literal_number(db, diagnostics, node, file_id)
            }

            SyntaxKind::TerminalShortString => {
                let node = ast::TerminalShortString::from_syntax_node(db, node);
                validate_short_string(db, diagnostics, node, file_id)
            }

            SyntaxKind::TerminalString => {
                let node = ast::TerminalString::from_syntax_node(db, node);
                validate_string(db, diagnostics, node, file_id)
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
/// 1. Is parsable according to its radix.
/// 2. Has properly formatted suffix.
fn validate_literal_number(
    db: &dyn SyntaxGroup,
    diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
    node: ast::TerminalLiteralNumber,
    file_id: FileId,
) -> Maybe<()> {
    let mut result = Ok(());

    let text = node.text(db);

    let (text, ty) = match text.split_once('_') {
        Some((text, ty)) => (text, Some(ty)),
        None => (text.as_str(), None),
    };

    // Verify number value is parsable.
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
                kind: ParserDiagnosticKind::Missing {
                    kind: ParserDiagnosticKindMissing::LiteralSuffix,
                    parsing_context: "literal number".into(),
                },
            }));
        }
    }

    result
}

/// Validates that the short string literal is valid, after it is consumed by the parser.
///
/// Cairo parser tries to consume even not proper tokens in order to support code editions in IDEs.
/// This means that it omits some crucial details in the literals that make the code uncompilable.
/// This function validates that the literal:
/// 1. Ends with a quote (parser accepts unterminated literals).
/// 2. Has all escape sequences valid.
/// 3. Is entirely ASCII.
fn validate_short_string(
    db: &dyn SyntaxGroup,
    diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
    node: ast::TerminalShortString,
    file_id: FileId,
) -> Maybe<()> {
    validate_any_string(
        db,
        diagnostics,
        node.as_syntax_node(),
        file_id,
        '\'',
        ParserDiagnosticKind::UnterminatedShortString,
        ParserDiagnosticKind::ShortStringMustBeAscii,
    )
}

/// Validates that the string literal is valid, after it is consumed by the parser.
///
/// Cairo parser tries to consume even not proper tokens in order to support code editions in IDEs.
/// This means that it omits some crucial details in the literals that make the code uncompilable.
/// This function validates that the literal:
/// 1. Ends with double quotes (parser accepts unterminated literals).
/// 2. Has all escape sequences valid.
/// 3. Is entirely ASCII.
fn validate_string(
    db: &dyn SyntaxGroup,
    diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
    node: ast::TerminalString,
    file_id: FileId,
) -> Maybe<()> {
    validate_any_string(
        db,
        diagnostics,
        node.as_syntax_node(),
        file_id,
        '"',
        ParserDiagnosticKind::UnterminatedString,
        ParserDiagnosticKind::StringMustBeAscii,
    )
}

/// Validates a short-string/string.
fn validate_any_string(
    db: &dyn SyntaxGroup,
    diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
    syntax_node: SyntaxNode,
    file_id: FileId,
    delimiter: char,
    unterminated_string_diagnostic_kind: ParserDiagnosticKind,
    ascii_only_diagnostic_kind: ParserDiagnosticKind,
) -> Maybe<()> {
    let text = syntax_node.get_text(db);
    let (_, text) = text.split_once(delimiter).unwrap();

    let Some((body, _suffix)) = text.rsplit_once(delimiter) else {
        return Err(diagnostics.add(ParserDiagnostic {
            file_id,
            span: syntax_node.span(db),
            kind: unterminated_string_diagnostic_kind,
        }));
    };

    validate_string_body(db, diagnostics, body, file_id, syntax_node, ascii_only_diagnostic_kind)
}

fn validate_string_body(
    db: &dyn SyntaxGroup,
    diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
    body: &str,
    file_id: FileId,
    node: SyntaxNode,
    ascii_only_diagnostic_kind: ParserDiagnosticKind,
) -> Result<(), cairo_lang_diagnostics::DiagnosticAdded> {
    let body = match unescape(body) {
        Ok(body) => body,
        Err(_) => {
            // TODO(mkaput): Try to always provide full position for entire escape sequence.
            return Err(diagnostics.add(ParserDiagnostic {
                file_id,
                span: node.span(db),
                kind: ParserDiagnosticKind::IllegalStringEscaping,
            }));
        }
    };

    if !body.is_ascii() {
        // TODO(mkaput): Try to always provide position of culprit character/escape sequence.
        return Err(diagnostics.add(ParserDiagnostic {
            file_id,
            span: node.span(db),
            kind: ascii_only_diagnostic_kind,
        }));
    }

    Ok(())
}
