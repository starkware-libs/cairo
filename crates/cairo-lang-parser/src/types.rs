use std::fmt::Display;

use cairo_lang_filesystem::span::TextOffset;

/// This one describes a struct that can be threated as substitute to a plain code when using the
/// parser. Using it, we can control what the initial [TextOffset] should be used during creation of
/// a Root [cairo_lang_syntax::node::SyntaxNode].
pub trait TokenStream: Display {
    fn get_start_offset(&self) -> Option<TextOffset>;
}
