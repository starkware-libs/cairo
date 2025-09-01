//! This module implements syntax validation that the parser doesn't handle.
//!
//! A failed validation emits a diagnostic.

use num_bigint::BigInt;
use num_traits::Num;
use unescaper::unescape;

use crate::diagnostic::ParserDiagnosticKind;

/// The validation error that is returned by the validation functions.
pub struct ValidationError {
    /// The kind of the diagnostic returned.
    pub kind: ParserDiagnosticKind,
    /// The location of the diagnostic within the span.
    pub location: ValidationLocation,
}
impl ValidationError {
    /// Creates a validation error that includes the entire span.
    fn full(kind: ParserDiagnosticKind) -> Self {
        ValidationError { kind, location: ValidationLocation::Full }
    }
}

/// The location of the validation error within the span.
pub enum ValidationLocation {
    /// The error is at the entire span.
    Full,
    /// The error is at the end of the span, after the consumed token.
    After,
}

/// Validate that the numeric literal is valid, after it is consumed by the parser.
///
/// Cairo parser tries to consume even not proper tokens in order to support code editions in IDEs.
/// This means that it omits some crucial details in the literals that make the code uncompilable.
/// This function validates that the literal:
/// 1. Is parsable according to its radix.
/// 2. Has properly formatted suffix.
pub fn validate_literal_number(text: &str) -> Option<ValidationError> {
    let (text, ty) = match text.split_once('_') {
        Some((text, ty)) => (text, Some(ty)),
        None => (text, None),
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
            return Some(ValidationError::full(ParserDiagnosticKind::InvalidNumericLiteralValue));
        }
    }

    // Verify suffix.
    if let Some(ty) = ty
        && ty.is_empty()
    {
        Some(ValidationError {
            kind: ParserDiagnosticKind::MissingLiteralSuffix,
            location: ValidationLocation::After,
        })
    } else {
        None
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
pub fn validate_short_string(text: &str) -> Option<ValidationError> {
    validate_any_string(
        text,
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
pub fn validate_string(text: &str) -> Option<ValidationError> {
    validate_any_string(
        text,
        '"',
        ParserDiagnosticKind::UnterminatedString,
        ParserDiagnosticKind::StringMustBeAscii,
    )
}

/// Validates a short-string/string.
fn validate_any_string(
    text: &str,
    delimiter: char,
    unterminated_string_diagnostic_kind: ParserDiagnosticKind,
    ascii_only_diagnostic_kind: ParserDiagnosticKind,
) -> Option<ValidationError> {
    let (_, text) = text.split_once(delimiter).unwrap();

    let Some((body, _suffix)) = text.rsplit_once(delimiter) else {
        return Some(ValidationError::full(unterminated_string_diagnostic_kind));
    };

    validate_string_body(body, ascii_only_diagnostic_kind)
}

fn validate_string_body(
    body: &str,
    ascii_only_diagnostic_kind: ParserDiagnosticKind,
) -> Option<ValidationError> {
    let Ok(body) = unescape(body) else {
        // TODO(mkaput): Try to always provide full position for entire escape sequence.
        return Some(ValidationError::full(ParserDiagnosticKind::IllegalStringEscaping));
    };

    if !body.is_ascii() {
        // TODO(mkaput): Try to always provide position of culprit character/escape sequence.
        return Some(ValidationError::full(ascii_only_diagnostic_kind));
    }
    None
}
