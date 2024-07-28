use cairo_lang_filesystem::span::TextWidth;

use super::ids::{GreenId, TextId};
use super::kind::SyntaxKind;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GreenNodeDetails {
    Token(TextId),
    Node(Vec<GreenId>),
}

/// Green node. Underlying untyped representation of the syntax tree.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct GreenNode {
    pub kind: SyntaxKind,
    pub width: TextWidth,
    pub details: GreenNodeDetails,
}
impl GreenNode {
    pub fn children(&self) -> &[GreenId] {
        match &self.details {
            GreenNodeDetails::Token(_) => &[],
            GreenNodeDetails::Node(children) => children,
        }
    }
}
