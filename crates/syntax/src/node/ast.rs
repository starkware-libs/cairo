use super::kind::SyntaxKind;
use super::{GreenId, GreenInterner, GreenNode, GreenNodeInternal, SyntaxNode, TypedSyntaxNode};

// An AST node with no children.
#[allow(dead_code)]
pub struct Empty {
    node: SyntaxNode,
    children: Vec<SyntaxNode>,
}
impl Empty {
    pub fn new_green(db: &dyn GreenInterner) -> GreenId {
        let children: Vec<GreenId> = vec![];
        db.intern_green(GreenNode::Internal(GreenNodeInternal {
            kind: SyntaxKind::Empty,
            children,
            width: 0,
        }))
    }
}
impl TypedSyntaxNode for Empty {
    fn missing(db: &dyn GreenInterner) -> GreenId {
        let children: Vec<GreenId> = vec![];
        db.intern_green(GreenNode::Internal(GreenNodeInternal {
            kind: SyntaxKind::Empty,
            children,
            width: 0,
        }))
    }
    fn from_syntax_node(db: &dyn GreenInterner, node: SyntaxNode) -> Self {
        match db.lookup_intern_green(node.0.green) {
            GreenNode::Internal(GreenNodeInternal { kind: SyntaxKind::Empty, .. }) => {
                let children = node.children(db);
                Self { node, children }
            }
            node => {
                panic!("Unexpected node {:?}. Expected {:?}.", node, SyntaxKind::Empty);
            }
        }
    }
}
