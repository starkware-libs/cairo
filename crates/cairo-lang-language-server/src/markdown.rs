use std::borrow::Cow;
use std::fmt;
use std::ops::AddAssign;

use itertools::Itertools;

#[cfg(test)]
#[path = "markdown_test.rs"]
mod test;

/// A convenience wrapper for building Markdown texts for used to display rich text in the IDE.
///
/// Markdown is used because this is the format used by the LSP protocol for rich text.
#[derive(Default, Debug)]
pub struct Markdown {
    text: String,
}

/// Constructors and primitive operations.
impl Markdown {
    /// Creates a new [`Markdown`] instance with empty text.
    pub fn empty() -> Self {
        Default::default()
    }

    /// Horizontal rule.
    pub fn rule() -> Self {
        "\n---\n".into()
    }

    /// Creates a new [`Markdown`] instance with the given code surrounded with `cairo` fenced code
    /// block.
    pub fn fenced_code_block(contents: &str) -> Self {
        format!("```cairo\n{contents}\n```").into()
    }

    /// Appends the given Markdown text to the current text.
    pub fn append(&mut self, other: Self) {
        self.text.push_str(&other.text);
    }
}

impl From<Markdown> for String {
    fn from(markdown: Markdown) -> Self {
        markdown.text
    }
}

impl From<String> for Markdown {
    fn from(text: String) -> Self {
        Markdown { text }
    }
}

impl From<&str> for Markdown {
    fn from(text: &str) -> Self {
        text.to_owned().into()
    }
}

impl fmt::Display for Markdown {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.text, f)
    }
}

impl AddAssign for Markdown {
    fn add_assign(&mut self, rhs: Self) {
        self.append(rhs)
    }
}

/// High-level operations used throughout LS functionality.
impl Markdown {
    /// Adds `cairo` language code to all fenced code blocks in the text that do not specify
    /// language.
    pub fn convert_fenced_code_blocks_to_cairo(&mut self) {
        let mut in_cairo_fence = false;
        self.text = self
            .text
            .lines()
            .map(|line| match (line.strip_prefix("```"), in_cairo_fence) {
                // Start of a fenced code block without language code.
                (Some(rest), false) if rest.trim_start().is_empty() => {
                    in_cairo_fence = true;
                    Cow::Owned(format!("```cairo{rest}"))
                }
                // Start of a fenced code block but with some language code.
                (Some(_), false) => {
                    in_cairo_fence = true;
                    Cow::Borrowed(line)
                }
                // End of a fenced code block.
                (Some(_), true) => {
                    in_cairo_fence = false;
                    Cow::Borrowed(line)
                }
                // Unrelated line.
                (None, _) => Cow::Borrowed(line),
            })
            .join("\n");
    }

    /// Ensures that the text ends with `\n`.
    pub fn ensure_trailing_newline(&mut self) {
        if !self.text.ends_with('\n') {
            self.text.push('\n');
        }
    }
}
