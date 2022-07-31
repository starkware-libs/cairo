use crate::node::kind::SyntaxKind;
use crate::token::TokenKind;

#[derive(Debug, PartialEq)]
pub enum NodeKind {
    Token(TokenKind),
    AnyToken,
    Internal(SyntaxKind),
}

pub trait ToNodeKind {
    fn to_node_kind(&self) -> NodeKind;
    fn to_node_kind_vec(&self) -> Vec<NodeKind> {
        vec![self.to_node_kind()]
    }
}
impl ToNodeKind for TokenKind {
    fn to_node_kind(&self) -> NodeKind {
        NodeKind::Token(*self)
    }
}
impl ToNodeKind for SyntaxKind {
    fn to_node_kind(&self) -> NodeKind {
        NodeKind::Internal(*self)
    }
}
