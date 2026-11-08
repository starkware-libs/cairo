//! Custom methods for AST node types.
//!
//! The impls here are visible through [`super`] module.

use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_utils::require;
use num_bigint::{BigInt, Sign};
use num_traits::Num;
use salsa::Database;
use unescaper::unescape;

use super::{
    TerminalFalse, TerminalLiteralNumber, TerminalShortString, TerminalString, TerminalTrue,
};
use crate::node::Terminal;

impl<'a> TerminalTrue<'a> {
    #[inline(always)]
    pub fn boolean_value(&self) -> bool {
        true
    }
}

impl<'a> TerminalFalse<'a> {
    #[inline(always)]
    pub fn boolean_value(&self) -> bool {
        false
    }
}

impl<'a> TerminalLiteralNumber<'a> {
    /// Interpret this terminal as a [`BigInt`] number and get the suffix if this literal has one.
    ///
    /// Splits at `_` boundaries from right to left, returning the longest prefix that parses as a
    /// valid number in the appropriate radix, with the remainder (after the `_`) as the suffix.
    /// This handles ambiguity for radices where suffix chars are valid digits — e.g., `0x1_f32`
    /// parses as hex `0x1F32` (no suffix) because `"1_f32"` is a valid hex number, while
    /// `1_f32` in decimal returns value `1` with suffix `"f32"`. Also handles compound suffixes
    /// like `1_u32_u8`: returns value `1` with suffix `"u32_u8"`.
    ///
    /// Returns `(BigInt::ZERO, Some(full_token_text))` as a sentinel for malformed literals where
    /// no prefix parses (e.g., `0x_u32` where the hex body is empty). Callers treat `Some` suffix
    /// as an error indicator.
    pub fn numeric_value_and_suffix(
        &self,
        db: &'a dyn Database,
    ) -> (BigInt, Option<SmolStrId<'a>>) {
        let text = self.text(db).long(db).as_str();

        let (text, radix) = if let Some(num_no_prefix) = text.strip_prefix("0x") {
            (num_no_prefix, 16)
        } else if let Some(num_no_prefix) = text.strip_prefix("0o") {
            (num_no_prefix, 8)
        } else if let Some(num_no_prefix) = text.strip_prefix("0b") {
            (num_no_prefix, 2)
        } else {
            (text, 10)
        };

        let mut index = text.len();
        while index > 0 {
            if let Ok(value) = BigInt::from_str_radix(&text[..index], radix) {
                return (value, text.get((index + 1)..).map(|s| SmolStrId::from(db, s)));
            }
            index = text[..index].rfind('_').unwrap_or(0);
        }
        // Malformed token (e.g., `0x_u32`): no numeric prefix found; return full text as suffix
        // so callers that check `suffix.is_some()` correctly treat this as an error.
        (BigInt::ZERO, Some(self.text(db)))
    }
}

impl<'a> TerminalShortString<'a> {
    /// Interpret this token/terminal as a string.
    pub fn string_value(&self, db: &'a dyn Database) -> Option<String> {
        let text = self.text(db).long(db);

        let (text, _suffix) = string_value(text, '\'')?;

        Some(text)
    }

    /// Interpret this terminal as a [`BigInt`] number.
    pub fn numeric_value(&self, db: &'a dyn Database) -> Option<BigInt> {
        self.string_value(db).map(|string| BigInt::from_bytes_be(Sign::Plus, string.as_bytes()))
    }

    /// Get suffix from this literal if it has one.
    pub fn suffix(&self, db: &'a dyn Database) -> Option<&'a str> {
        let text = self.text(db).long(db);
        let (_literal, mut suffix) = text[1..].rsplit_once('\'')?;
        require(!suffix.is_empty())?;
        if suffix.starts_with('_') {
            suffix = &suffix[1..];
        }
        Some(suffix)
    }
}

impl<'a> TerminalString<'a> {
    /// Interpret this token/terminal as a string.
    pub fn string_value(&self, db: &'a dyn Database) -> Option<String> {
        let text = self.text(db).long(db);
        let (text, suffix) = string_value(text, '"')?;
        if !suffix.is_empty() {
            unreachable!();
        }

        Some(text)
    }
}

/// Interpret the given text as a string with the given delimiter. Returns the text and the suffix.
fn string_value(text: &str, delimiter: char) -> Option<(String, &str)> {
    let (prefix, text) = text.split_once(delimiter)?;
    if !prefix.is_empty() {
        unreachable!();
    }

    let (text, suffix) = text.rsplit_once(delimiter)?;

    let text = unescape(text).ok()?;

    require(text.is_ascii())?;

    Some((text, suffix))
}
