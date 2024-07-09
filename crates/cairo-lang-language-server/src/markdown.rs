//! Utilities for building Markdown texts.
//!
//! Markdown is used because this is the format used by the LSP protocol for rich text.

/// Horizontal rule.
pub const RULE: &str = "---\n";

/// Surround the given code with `cairo` fenced code block.
pub fn fenced_code_block(code: &str) -> String {
    fenced_code_block_lang("cairo", code)
}

/// Surround the given code with a fenced code block of specified language.
pub fn fenced_code_block_lang(lang: &str, code: &str) -> String {
    format!("```{lang}\n{code}\n```\n")
}
