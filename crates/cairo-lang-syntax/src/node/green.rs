use smol_str::SmolStr;

use super::ids::GreenId;
use super::kind::SyntaxKind;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GreenNodeDetails {
    Token(SmolStr),
    Node { children: Vec<GreenId>, width: u32 },
}
/// Green node. Underlying untyped representation of the syntax tree.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct GreenNode {
    pub kind: SyntaxKind,
    pub details: GreenNodeDetails,
}
impl GreenNode {
    pub fn width(&self) -> u32 {
        match &self.details {
            GreenNodeDetails::Token(text) => text.len() as u32,
            GreenNodeDetails::Node { width, .. } => *width,
        }
    }
    pub fn children(self) -> Vec<GreenId> {
        match self.details {
            GreenNodeDetails::Token(_text) => Vec::new(),
            GreenNodeDetails::Node { children, .. } => children,
        }
    }
}
