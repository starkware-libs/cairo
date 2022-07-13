use super::element_list::ElementList;
use super::kind::SyntaxKind;
use super::{
    GreenId, GreenInternalNode, GreenInterner, GreenNode, SyntaxNode, SyntaxToken, TypedSyntaxNode,
};
use std::ops::Deref;
pub struct Empty {
    node: SyntaxNode,
    children: Vec<SyntaxNode>,
}
impl Empty {
    pub fn new_green(db: &dyn GreenInterner) -> GreenId {
        let children: Vec<GreenId> = vec![];
        let width = children.iter().map(|id| db.lookup_intern_green(*id).width()).sum();
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
pub struct Terminal {
    node: SyntaxNode,
    children: Vec<SyntaxNode>,
}
impl Terminal {
    pub fn new_green(
        db: &dyn GreenInterner,
        leading_trivia: GreenId,
        token: GreenId,
        trailing_trivia: GreenId,
    ) -> GreenId {
        let children: Vec<GreenId> = vec![leading_trivia, token, trailing_trivia];
        let width = children.iter().map(|id| db.lookup_intern_green(*id).width()).sum();
        db.intern_green(GreenNode::Internal(GreenInternalNode {
            kind: SyntaxKind::Terminal,
            children,
            width,
        }))
    }
    pub fn leading_trivia(&self, db: &dyn GreenInterner) -> Trivia {
        let child = self.children[0].clone();
        Trivia::from_syntax_node(db, child)
    }
    pub fn token(&self, db: &dyn GreenInterner) -> SyntaxToken {
        let child = self.children[1].clone();
        SyntaxToken::from_syntax_node(db, child)
    }
    pub fn trailing_trivia(&self, db: &dyn GreenInterner) -> Trivia {
        let child = self.children[2].clone();
        Trivia::from_syntax_node(db, child)
    }
}
impl TypedSyntaxNode for Terminal {
    fn missing(db: &dyn GreenInterner) -> GreenId {
        let children: Vec<GreenId> =
            vec![Trivia::missing(db), SyntaxToken::missing(db), Trivia::missing(db)];
        db.intern_green(GreenNode::Internal(GreenInternalNode {
            kind: SyntaxKind::Terminal,
            children,
            width: 0,
        }))
    }
    fn from_syntax_node(db: &dyn GreenInterner, node: SyntaxNode) -> Self {
        let green = db.lookup_intern_green(node.0.green);
        match green {
            GreenNode::Internal(internal) => {
                if internal.kind != SyntaxKind::Terminal {
                    panic!(
                        "Unexpected SyntaxKind {:?}. Expected {:?}.",
                        internal.kind,
                        SyntaxKind::Terminal
                    );
                }
                let children = node.children(db);
                Self { node, children }
            }
            GreenNode::Token(token) => {
                panic!("Unexpected Token {:?}. Expected {:?}.", token, SyntaxKind::Terminal);
            }
        }
    }
}
pub struct Trivia(ElementList<Trivium, 1>);
impl Deref for Trivia {
    type Target = ElementList<Trivium, 1>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl Trivia {
    pub fn new_green(db: &dyn GreenInterner, children: Vec<GreenId>) -> GreenId {
        let width = children.iter().map(|id| db.lookup_intern_green(*id).width()).sum();
        db.intern_green(GreenNode::Internal(GreenInternalNode {
            kind: SyntaxKind::Trivia,
            children,
            width,
        }))
    }
}
impl TypedSyntaxNode for Trivia {
    fn missing(db: &dyn GreenInterner) -> GreenId {
        db.intern_green(GreenNode::Internal(GreenInternalNode {
            kind: SyntaxKind::Trivia,
            children: vec![],
            width: 0,
        }))
    }
    fn from_syntax_node(_db: &dyn GreenInterner, node: SyntaxNode) -> Self {
        Self(ElementList::new(node))
    }
}
pub enum Trivium {
    TriviumSingleLineComment(TriviumSingleLineComment),
    TriviumWhitespace(TriviumWhitespace),
    TriviumNewline(TriviumNewline),
    Empty(Empty),
}
impl TypedSyntaxNode for Trivium {
    fn missing(db: &dyn GreenInterner) -> GreenId {
        db.intern_green(GreenNode::Internal(GreenInternalNode {
            kind: SyntaxKind::Empty,
            children: vec![],
            width: 0,
        }))
    }
    fn from_syntax_node(db: &dyn GreenInterner, node: SyntaxNode) -> Self {
        let green = db.lookup_intern_green(node.0.green);
        if let GreenNode::Internal(internal) = green {
            match internal.kind {
                SyntaxKind::TriviumSingleLineComment => {
                    return Trivium::TriviumSingleLineComment(
                        TriviumSingleLineComment::from_syntax_node(db, node),
                    )
                }
                SyntaxKind::TriviumWhitespace => {
                    return Trivium::TriviumWhitespace(TriviumWhitespace::from_syntax_node(
                        db, node,
                    ))
                }
                SyntaxKind::TriviumNewline => {
                    return Trivium::TriviumNewline(TriviumNewline::from_syntax_node(db, node))
                }
                SyntaxKind::Empty => return Trivium::Empty(Empty::from_syntax_node(db, node)),
                _ => {
                    panic!("Unexpected syntax kind {:?} when constructing $(&name).", internal.kind)
                }
            }
        }
        panic!("Expected an internal node.");
    }
}
pub struct TriviumSingleLineComment {
    node: SyntaxNode,
    children: Vec<SyntaxNode>,
}
impl TriviumSingleLineComment {
    pub fn new_green(db: &dyn GreenInterner, token: GreenId) -> GreenId {
        let children: Vec<GreenId> = vec![token];
        let width = children.iter().map(|id| db.lookup_intern_green(*id).width()).sum();
        db.intern_green(GreenNode::Internal(GreenInternalNode {
            kind: SyntaxKind::TriviumSingleLineComment,
            children,
            width,
        }))
    }
    pub fn token(&self, db: &dyn GreenInterner) -> SyntaxToken {
        let child = self.children[0].clone();
        SyntaxToken::from_syntax_node(db, child)
    }
}
impl TypedSyntaxNode for TriviumSingleLineComment {
    fn missing(db: &dyn GreenInterner) -> GreenId {
        let children: Vec<GreenId> = vec![SyntaxToken::missing(db)];
        db.intern_green(GreenNode::Internal(GreenInternalNode {
            kind: SyntaxKind::TriviumSingleLineComment,
            children,
            width: 0,
        }))
    }
    fn from_syntax_node(db: &dyn GreenInterner, node: SyntaxNode) -> Self {
        let green = db.lookup_intern_green(node.0.green);
        match green {
            GreenNode::Internal(internal) => {
                if internal.kind != SyntaxKind::TriviumSingleLineComment {
                    panic!(
                        "Unexpected SyntaxKind {:?}. Expected {:?}.",
                        internal.kind,
                        SyntaxKind::TriviumSingleLineComment
                    );
                }
                let children = node.children(db);
                Self { node, children }
            }
            GreenNode::Token(token) => {
                panic!(
                    "Unexpected Token {:?}. Expected {:?}.",
                    token,
                    SyntaxKind::TriviumSingleLineComment
                );
            }
        }
    }
}
pub struct TriviumWhitespace {
    node: SyntaxNode,
    children: Vec<SyntaxNode>,
}
impl TriviumWhitespace {
    pub fn new_green(db: &dyn GreenInterner, token: GreenId) -> GreenId {
        let children: Vec<GreenId> = vec![token];
        let width = children.iter().map(|id| db.lookup_intern_green(*id).width()).sum();
        db.intern_green(GreenNode::Internal(GreenInternalNode {
            kind: SyntaxKind::TriviumWhitespace,
            children,
            width,
        }))
    }
    pub fn token(&self, db: &dyn GreenInterner) -> SyntaxToken {
        let child = self.children[0].clone();
        SyntaxToken::from_syntax_node(db, child)
    }
}
impl TypedSyntaxNode for TriviumWhitespace {
    fn missing(db: &dyn GreenInterner) -> GreenId {
        let children: Vec<GreenId> = vec![SyntaxToken::missing(db)];
        db.intern_green(GreenNode::Internal(GreenInternalNode {
            kind: SyntaxKind::TriviumWhitespace,
            children,
            width: 0,
        }))
    }
    fn from_syntax_node(db: &dyn GreenInterner, node: SyntaxNode) -> Self {
        let green = db.lookup_intern_green(node.0.green);
        match green {
            GreenNode::Internal(internal) => {
                if internal.kind != SyntaxKind::TriviumWhitespace {
                    panic!(
                        "Unexpected SyntaxKind {:?}. Expected {:?}.",
                        internal.kind,
                        SyntaxKind::TriviumWhitespace
                    );
                }
                let children = node.children(db);
                Self { node, children }
            }
            GreenNode::Token(token) => {
                panic!(
                    "Unexpected Token {:?}. Expected {:?}.",
                    token,
                    SyntaxKind::TriviumWhitespace
                );
            }
        }
    }
}
pub struct TriviumNewline {
    node: SyntaxNode,
    children: Vec<SyntaxNode>,
}
impl TriviumNewline {
    pub fn new_green(db: &dyn GreenInterner, token: GreenId) -> GreenId {
        let children: Vec<GreenId> = vec![token];
        let width = children.iter().map(|id| db.lookup_intern_green(*id).width()).sum();
        db.intern_green(GreenNode::Internal(GreenInternalNode {
            kind: SyntaxKind::TriviumNewline,
            children,
            width,
        }))
    }
    pub fn token(&self, db: &dyn GreenInterner) -> SyntaxToken {
        let child = self.children[0].clone();
        SyntaxToken::from_syntax_node(db, child)
    }
}
impl TypedSyntaxNode for TriviumNewline {
    fn missing(db: &dyn GreenInterner) -> GreenId {
        let children: Vec<GreenId> = vec![SyntaxToken::missing(db)];
        db.intern_green(GreenNode::Internal(GreenInternalNode {
            kind: SyntaxKind::TriviumNewline,
            children,
            width: 0,
        }))
    }
    fn from_syntax_node(db: &dyn GreenInterner, node: SyntaxNode) -> Self {
        let green = db.lookup_intern_green(node.0.green);
        match green {
            GreenNode::Internal(internal) => {
                if internal.kind != SyntaxKind::TriviumNewline {
                    panic!(
                        "Unexpected SyntaxKind {:?}. Expected {:?}.",
                        internal.kind,
                        SyntaxKind::TriviumNewline
                    );
                }
                let children = node.children(db);
                Self { node, children }
            }
            GreenNode::Token(token) => {
                panic!("Unexpected Token {:?}. Expected {:?}.", token, SyntaxKind::TriviumNewline);
            }
        }
    }
}
