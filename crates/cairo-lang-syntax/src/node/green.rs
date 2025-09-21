use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_filesystem::span::TextWidth;
use salsa::Database;

use super::ids::GreenId;
use super::kind::SyntaxKind;

#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::Update)]
pub enum GreenNodeDetails<'a> {
    Token(SmolStrId<'a>),
    Node { children: Vec<GreenId<'a>>, width: TextWidth },
}
/// Green node. Underlying untyped representation of the syntax tree.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct GreenNode<'a> {
    pub kind: SyntaxKind,
    pub details: GreenNodeDetails<'a>,
}
impl<'a> GreenNode<'a> {
    pub fn width(&self, db: &'a dyn Database) -> TextWidth {
        match &self.details {
            GreenNodeDetails::Token(text) => TextWidth::from_str(text.long(db)),
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
