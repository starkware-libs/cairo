use std::fmt;
use std::ops::AddAssign;

/// A convenience wrapper for building Markdown texts for used to display rich text in the IDE.
///
/// Markdown is used because this is the format used by the LSP protocol for rich text.
#[derive(Clone, Default, Debug)]
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
        "---\n".into()
    }

    /// Creates a new [`Markdown`] instance with the given code surrounded with `cairo` fenced code
    /// block.
    pub fn fenced_code_block(contents: &str) -> Self {
        format!("```cairo\n{contents}\n```\n").into()
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
