#![deny(missing_docs)]
//! This crate defines unfiorm and primitive form of the TokenStream.
//! We want this to be as stable as possible and limit the changes here to bare minimum.

/// Stable representation of a token's span.
pub struct StableSpan {
    /// Start position of the span.
    pub start: usize,
    /// End position of the span.
    pub end: usize,
}

/// Stable representation of a single token.
pub struct StableToken {
    /// Plain code content that the token represents (includes whitespaces).
    pub content: String,
    /// Span of the token.
    pub span: Option<StableSpan>,
}

impl StableToken {
    /// Creates a new stable token based upon content and provided span.
    pub fn new(content: String, span: Option<StableSpan>) -> Self {
        Self { content, span }
    }
}

/// Trait that defines an object that can be turned into a StableTokenStream iterator.
pub trait ToStableTokenStream {
    /// Iterator type for StableTokens.
    type Iter: Iterator<Item = StableToken>;

    /// Method that turns given item to a StableTokenStream iterator.
    fn to_stable_token_stream(&self) -> Self::Iter;
}
