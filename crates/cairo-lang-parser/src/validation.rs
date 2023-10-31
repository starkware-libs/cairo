//! This module implements syntax validation that the parser doesn't handle.
//!
//! A failed validation emits a diagnostic.

use cairo_lang_diagnostics::DiagnosticsBuilder;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::TextSpan;
use num_bigint::BigInt;
use num_traits::Num;
use smol_str::SmolStr;
use unescaper::unescape;

use crate::diagnostic::ParserDiagnosticKind;
use crate::ParserDiagnostic;

/// Validate that the numeric literal is valid, after it is consumed by the parser.
///
/// Cairo parser tries to consume even not proper tokens in order to support code editions in IDEs.
/// This means that it omits some crucial details in the literals that make the code uncompilable.
/// This function validates that the literal:
/// 1. Is parsable according to its radix.
/// 2. Has properly formatted suffix.
pub fn validate_literal_number(
    diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
    text: SmolStr,
    span: TextSpan,
    file_id: FileId,
) {
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
            diagnostics.add(ParserDiagnostic {
                file_id,
                span,
                kind: ParserDiagnosticKind::InvalidNumericLiteralValue,
            });
        }
    }

    // Verify suffix.
    if let Some(ty) = ty {
        if ty.is_empty() {
            diagnostics.add(ParserDiagnostic {
                file_id,
                span: span.after(),
                kind: ParserDiagnosticKind::MissingLiteralSuffix,
            });
        }
    }
}

/// Validates that the short string literal is valid, after it is consumed by the parser.
///
/// Cairo parser tries to consume even not proper tokens in order to support code editions in IDEs.
/// This means that it omits some crucial details in the literals that make the code uncompilable.
/// This function validates that the literal:
/// 1. Ends with a quote (parser accepts unterminated literals).
/// 2. Has all escape sequences valid.
/// 3. Is entirely ASCII.
pub fn validate_short_string(
    diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
    text: SmolStr,
    span: TextSpan,
    file_id: FileId,
) {
    validate_any_string(
        diagnostics,
        text,
        span,
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
pub fn validate_string(
    diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
    text: SmolStr,
    span: TextSpan,
    file_id: FileId,
) {
    validate_any_string(
        diagnostics,
        text,
        span,
        file_id,
        '"',
        ParserDiagnosticKind::UnterminatedString,
        ParserDiagnosticKind::StringMustBeAscii,
    )
}

/// Validates a short-string/string.
fn validate_any_string(
    diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
    text: SmolStr,
    span: TextSpan,
    file_id: FileId,
    delimiter: char,
    unterminated_string_diagnostic_kind: ParserDiagnosticKind,
    ascii_only_diagnostic_kind: ParserDiagnosticKind,
) {
    let (_, text) = text.split_once(delimiter).unwrap();

    let Some((body, _suffix)) = text.rsplit_once(delimiter) else {
        diagnostics.add(ParserDiagnostic {
            file_id,
            span,
            kind: unterminated_string_diagnostic_kind,
        });
        return;
    };

    validate_string_body(diagnostics, body, span, file_id, ascii_only_diagnostic_kind)
}

fn validate_string_body(
    diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
    body: &str,
    span: TextSpan,
    file_id: FileId,
    ascii_only_diagnostic_kind: ParserDiagnosticKind,
) {
    let Ok(body) = unescape(body) else {
        // TODO(mkaput): Try to always provide full position for entire escape sequence.
        diagnostics.add(ParserDiagnostic {
            file_id,
            span,
            kind: ParserDiagnosticKind::IllegalStringEscaping,
        });
        return;
    };

    if !body.is_ascii() {
        // TODO(mkaput): Try to always provide position of culprit character/escape sequence.
        diagnostics.add(ParserDiagnostic { file_id, span, kind: ascii_only_diagnostic_kind });
    }
}
