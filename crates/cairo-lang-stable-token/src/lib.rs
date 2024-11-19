//! This crate defines unfiorm and primitive form of the TokenStream.
//! We want this to be as stable as possible and limit the changes here to bare minimum.

pub struct StableSpan {
    pub start: usize,
    pub end: usize,
}

pub struct StableToken(pub String, pub Option<StableSpan>);

impl StableToken {
    pub fn new(content: String, span: Option<StableSpan>) -> Self {
        Self(content, span)
    }
}

pub trait ToStableTokenStream {
    fn to_stable_token_stream(&self) -> impl Iterator<Item = StableToken>;
}
