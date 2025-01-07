#![deny(missing_docs)]
//! This crate defines uniform and primitive form of the TokenStream.
//! We want this to be as stable as possible and limit the changes here to bare minimum.

/// Primitive representation of a token's span.
pub struct PrimitiveSpan {
    /// Start position of the span.
    pub start: usize,
    /// End position of the span.
    pub end: usize,
}

/// Primitive representation of a single token.
pub struct PrimitiveToken {
    /// Plain code content that the token represents (includes whitespaces).
    pub content: String,
    /// Span of the token.
    pub span: Option<PrimitiveSpan>,
}

impl PrimitiveToken {
    /// Creates a new primitive token based upon content and provided span.
    pub fn new(content: String, span: Option<PrimitiveSpan>) -> Self {
        Self { content, span }
    }
}

/// Trait that defines an object that can be turned into a PrimitiveTokenStream iterator.
pub trait ToPrimitiveTokenStream {
    /// Iterator type for PrimitiveTokens.
    type Iter: Iterator<Item = PrimitiveToken>;

    /// Method that turns given item to a PrimitiveTokenStream iterator.
    fn to_primitive_token_stream(&self) -> Self::Iter;
}
