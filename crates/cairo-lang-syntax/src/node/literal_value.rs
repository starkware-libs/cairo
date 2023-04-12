use num_bigint::{BigInt, Sign};
use num_traits::Num;
use smol_str::SmolStr;
use unescaper::unescape;

use crate::node::ast::{TerminalFalse, TerminalLiteralNumber, TerminalShortString, TerminalTrue};
use crate::node::db::SyntaxGroup;
use crate::node::Terminal;

/// Error type returned from literal value traits methods.
#[derive(thiserror::Error, Debug)]
pub enum LiteralValueError {
    #[error("Invalid numeric literal.")]
    ParseBigIntError(#[from] num_bigint::ParseBigIntError),
    #[error("Invalid string escaping:\n{0}")]
    UnescapeError(#[from] unescaper::Error),
    #[error("Short strings can only include ASCII characters.")]
    ShortStringMustBeAscii,
}

/// `Result<T, LiteralValueError>`
pub type LiteralValueResult<T, E = LiteralValueError> = Result<T, E>;

/// Extension trait providing value interpretation methods for boolean literals.
pub trait BooleanValue {
    /// Interpret this token/terminal as a boolean value (`true`/`false`).
    fn boolean_value(&self) -> bool;
}

impl BooleanValue for TerminalTrue {
    fn boolean_value(&self) -> bool {
        true
    }
}

impl BooleanValue for TerminalFalse {
    fn boolean_value(&self) -> bool {
        false
    }
}

/// Extension trait providing value interpretation methods for numeric literals.
pub trait NumericValue {
    /// Interpret this token/terminal as a [`BigInt`] number.
    fn bigint_value(&self, db: &dyn SyntaxGroup) -> LiteralValueResult<BigInt>;
}

impl NumericValue for TerminalLiteralNumber {
    fn bigint_value(&self, db: &dyn SyntaxGroup) -> LiteralValueResult<BigInt> {
        let text = self.text(db);

        let text = match text.split_once('_') {
            Some((text, _ty)) => text,
            None => &text,
        };

        let (text, radix) = if let Some(num_no_prefix) = text.strip_prefix("0x") {
            (num_no_prefix, 16)
        } else if let Some(num_no_prefix) = text.strip_prefix("0o") {
            (num_no_prefix, 8)
        } else if let Some(num_no_prefix) = text.strip_prefix("0b") {
            (num_no_prefix, 2)
        } else {
            (text, 10)
        };

        BigInt::from_str_radix(text, radix).map_err(Into::into)
    }
}

impl NumericValue for TerminalShortString {
    fn bigint_value(&self, db: &dyn SyntaxGroup) -> LiteralValueResult<BigInt> {
        let string = self.string_value(db)?;
        if !string.is_ascii() {
            return Err(LiteralValueError::ShortStringMustBeAscii);
        }
        Ok(BigInt::from_bytes_be(Sign::Plus, string.as_bytes()))
    }
}

/// Extension trait providing value interpretation methods for string-like literals.
pub trait StringValue {
    /// Interpret this token/terminal as a string.
    fn string_value(&self, db: &dyn SyntaxGroup) -> unescaper::Result<String>;
}

impl StringValue for TerminalShortString {
    fn string_value(&self, db: &dyn SyntaxGroup) -> unescaper::Result<String> {
        let text = self.text(db);

        let mut text = text.as_str();
        if text.starts_with('\'') {
            (_, text) = text.split_once('\'').unwrap();
        }
        if let Some((body, _suffix)) = text.rsplit_once('\'') {
            text = body;
        }

        unescape(text)
    }
}

/// Extension trait providing suffix extraction methods from literals that can have ones.
pub trait SuffixedValue {
    /// Get suffix from this literal if it has one.
    fn suffix(&self, db: &dyn SyntaxGroup) -> Option<SmolStr>;
}

impl SuffixedValue for TerminalLiteralNumber {
    fn suffix(&self, db: &dyn SyntaxGroup) -> Option<SmolStr> {
        let text = self.text(db);
        let (_literal, ty) = text.rsplit_once('_')?;
        if ty.is_empty() {
            return None;
        }
        Some(ty.into())
    }
}

impl SuffixedValue for TerminalShortString {
    fn suffix(&self, db: &dyn SyntaxGroup) -> Option<SmolStr> {
        let text = self.text(db);
        let (_literal, suffix) = text[1..].rsplit_once('\'')?;
        if suffix.is_empty() {
            return None;
        }
        assert!(suffix.starts_with('_'), "short string suffix must start with underscore");
        Some(suffix[1..].into())
    }
}
