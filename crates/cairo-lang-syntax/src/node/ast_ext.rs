//! Custom methods for AST node types.
//!
//! The impls here are visible through [`super`] module.

use num_bigint::{BigInt, Sign};
use num_traits::Num;
use smol_str::SmolStr;
use unescaper::unescape;

use super::{TerminalFalse, TerminalLiteralNumber, TerminalShortString, TerminalTrue};
use crate::node::db::SyntaxGroup;
use crate::node::Terminal;

impl TerminalTrue {
    #[inline(always)]
    pub fn boolean_value(&self) -> bool {
        true
    }
}

impl TerminalFalse {
    #[inline(always)]
    pub fn boolean_value(&self) -> bool {
        false
    }
}

impl TerminalLiteralNumber {
    /// Interpret this terminal as a [`BigInt`] number.
    pub fn numeric_value(&self, db: &dyn SyntaxGroup) -> Option<BigInt> {
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

        BigInt::from_str_radix(text, radix).ok()
    }

    /// Get suffix from this literal if it has one.
    pub fn suffix(&self, db: &dyn SyntaxGroup) -> Option<SmolStr> {
        let text = self.text(db);
        let (_literal, ty) = text.rsplit_once('_')?;
        if ty.is_empty() {
            return None;
        }
        Some(ty.into())
    }
}

impl TerminalShortString {
    /// Interpret this token/terminal as a string.
    pub fn string_value(&self, db: &dyn SyntaxGroup) -> Option<String> {
        let text = self.text(db);

        let mut text = text.as_str();
        if text.starts_with('\'') {
            (_, text) = text.split_once('\'').unwrap();
        }
        if let Some((body, _suffix)) = text.rsplit_once('\'') {
            text = body;
        }

        let text = unescape(text).ok()?;

        if !text.is_ascii() {
            return None;
        }

        Some(text)
    }

    /// Interpret this terminal as a [`BigInt`] number.
    pub fn numeric_value(&self, db: &dyn SyntaxGroup) -> Option<BigInt> {
        self.string_value(db).map(|string| BigInt::from_bytes_be(Sign::Plus, string.as_bytes()))
    }

    /// Get suffix from this literal if it has one.
    pub fn suffix(&self, db: &dyn SyntaxGroup) -> Option<SmolStr> {
        let text = self.text(db);
        let (_literal, mut suffix) = text[1..].rsplit_once('\'')?;
        if suffix.is_empty() {
            return None;
        }
        if suffix.starts_with('_') {
            suffix = &suffix[1..];
        }
        Some(suffix.into())
    }
}
