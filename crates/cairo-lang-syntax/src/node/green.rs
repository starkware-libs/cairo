use cairo_lang_filesystem::ids::Span;
use cairo_lang_filesystem::span::TextWidth;

use super::ids::GreenId;
use super::kind::SyntaxKind;

#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::Update)]
pub enum GreenNodeDetails<'a> {
    Token(Span<'a>),
    Node { children: Vec<GreenId<'a>>, width: TextWidth },
}
/// Green node. Underlying untyped representation of the syntax tree.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct GreenNode<'a> {
    pub kind: SyntaxKind,
    pub details: GreenNodeDetails<'a>,
}
impl<'a> GreenNode<'a> {
    pub fn width(&self) -> TextWidth {
        match &self.details {
            GreenNodeDetails::Token(text) => text.width(),
            GreenNodeDetails::Node { width, .. } => *width,
        }
    }
    pub fn children(&self) -> &[GreenId<'a>] {
        match &self.details {
            GreenNodeDetails::Token(_text) => &[],
            GreenNodeDetails::Node { children, .. } => children,
        }
    }
}
