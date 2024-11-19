use std::fmt::Display;

use cairo_lang_filesystem::span::TextOffset;
use cairo_lang_syntax::node::db::SyntaxGroup;

pub trait Tokenable {
    fn len(&self, db: &dyn SyntaxGroup) -> u32;
}

/// Used to run Cairo language parser over a custom source of tokens not produced by the lexer.
pub trait TokenStream: Display + Tokenable {
    /// Returns the starting [`TextOffset`] of the token stream, if there is at least a single
    /// token.
    ///
    /// This property is used as offset of a root [cairo_lang_syntax::node::SyntaxNode] produced
    /// when parsing this stream.
    fn get_start_offset(&self) -> Option<TextOffset>;
}
