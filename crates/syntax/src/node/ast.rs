use super::kind::SyntaxKind;
use super::{GreenId, GreenInternalNode, GreenInterner, GreenNode, SyntaxNode, TypedSyntaxNode};
#[allow(dead_code)]
pub struct Empty {
    node: SyntaxNode,
    children: Vec<SyntaxNode>,
}
impl Empty {
    pub fn new_green(db: &dyn GreenInterner) -> GreenId {
        let children: Vec<GreenId> = vec![];
        let width = 0;
        db.intern_green(GreenNode::Internal(GreenInternalNode {
            kind: SyntaxKind::Empty,
            children,
            width,
        }))
    }
}
impl TypedSyntaxNode for Empty {
    fn missing(db: &dyn GreenInterner) -> GreenId {
        let children: Vec<GreenId> = vec![];
        db.intern_green(GreenNode::Internal(GreenInternalNode {
            kind: SyntaxKind::Empty,
            children,
            width: 0,
        }))
    }
    fn from_syntax_node(db: &dyn GreenInterner, node: SyntaxNode) -> Self {
        let green = db.lookup_intern_green(node.0.green);
        match green {
            GreenNode::Internal(internal) => {
                if internal.kind != SyntaxKind::Empty {
                    panic!(
                        "Unexpected SyntaxKind {:?}. Expected {:?}.",
                        internal.kind,
                        SyntaxKind::Empty
                    );
                }
                let children = node.children(db);
                Self { node, children }
            }
            GreenNode::Token(token) => {
                panic!("Unexpected Token {:?}. Expected {:?}.", token, SyntaxKind::Empty);
            }
        }
    }
}
