use cairo_lang_filesystem::span::TextWidth;
use smol_str::SmolStr;

use super::ids::GreenId;
use super::kind::SyntaxKind;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GreenNodeDetails {
    Token(SmolStr),
    Node { children: Vec<GreenId>, width: TextWidth },
}
/// Green node. Underlying untyped representation of the syntax tree.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct GreenNode {
    pub kind: SyntaxKind,
    pub details: GreenNodeDetails,
}
impl GreenNode {
    pub fn width(&self) -> TextWidth {
        match &self.details {
            GreenNodeDetails::Token(text) => TextWidth::from_str(text),
            GreenNodeDetails::Node { width, .. } => *width,
        }
    }
    pub fn children(&self) -> &[GreenId] {
        match &self.details {
            GreenNodeDetails::Token(_text) => &[],
            GreenNodeDetails::Node { children, .. } => children,
        }
    }
}
