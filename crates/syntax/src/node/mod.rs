use core::hash::Hash;
use std::sync::Arc;

use smol_str::SmolStr;

use self::db::GreenInterner;
use self::green::GreenNode;
use self::ids::GreenId;
use self::kind::SyntaxKind;
use crate::token;

#[allow(clippy::too_many_arguments)]
#[allow(dead_code)]
#[allow(unused_variables)]
pub mod ast;
#[cfg(test)]
mod ast_test;
pub mod db;
pub mod element_list;
pub mod green;
pub mod ids;
pub mod kind;

// TODO: Children should be excluded from Eq and Hash of Typed nodes.

// SyntaxNode. Untyped view of the syntax tree. Adds parent() and offset() capabilities.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
struct SyntaxNodeInner {
    green: GreenId,
    offset: u32,
    parent: Option<SyntaxNode>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SyntaxNode(Arc<SyntaxNodeInner>);
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SyntaxNodeKind {
    Syntax(SyntaxKind),
    Token(token::Token),
}
impl SyntaxNode {
    pub fn new_root(green: GreenId) -> Self {
        let inner = SyntaxNodeInner { green, offset: 0, parent: None };
        Self(Arc::new(inner))
    }
    pub fn kind(&self, db: &dyn GreenInterner) -> SyntaxNodeKind {
        match db.lookup_intern_green(self.0.green) {
            GreenNode::Internal(internal) => SyntaxNodeKind::Syntax(internal.kind),
            GreenNode::Token(token) => SyntaxNodeKind::Token(token),
        }
    }
    pub fn offset(&self) -> u32 {
        self.0.offset
    }
    pub fn width(&self, db: &dyn GreenInterner) -> u32 {
        match db.lookup_intern_green(self.0.green) {
            GreenNode::Internal(internal) => internal.width,
            GreenNode::Token(token) => token.width(),
        }
    }
    pub fn children(&self, db: &dyn GreenInterner) -> Vec<SyntaxNode> {
        let mut offset: u32 = self.0.offset;
        match db.lookup_intern_green(self.0.green) {
            GreenNode::Internal(internal) => internal
                .children
                .into_iter()
                .map(move |c| {
                    let res = SyntaxNode(Arc::new(SyntaxNodeInner {
                        green: c,
                        offset,
                        parent: Some(self.clone()),
                    }));
                    let width = db.lookup_intern_green(c).width();
                    offset += width;
                    res
                })
                .collect(),
            GreenNode::Token(_) => vec![],
        }
    }
    pub fn parent(&self) -> Option<SyntaxNode> {
        self.0.parent.as_ref().cloned()
    }
}

// Trait for the typed view of the syntax tree. All the internal node implementations are under
// the ast module.
pub trait TypedSyntaxNode {
    fn missing(db: &dyn GreenInterner) -> GreenId;
    fn from_syntax_node(db: &dyn GreenInterner, node: SyntaxNode) -> Self;
    fn as_syntax_node(&self) -> SyntaxNode;
}

// Typed view for a token. Implements the typed view interface TypedSyntaxNode.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Token {
    node: SyntaxNode,
}

impl Token {
    pub fn new_green(db: &dyn GreenInterner, kind: token::TokenKind, text: SmolStr) -> GreenId {
        db.intern_green(GreenNode::Token(token::Token { kind, text }))
    }
    pub fn raw(&self, db: &dyn GreenInterner) -> token::Token {
        let green = db.lookup_intern_green(self.node.0.green);
        if let GreenNode::Token(token) = green {
            return token;
        }
        panic!("Expected a token.");
    }
    pub fn kind(&self, db: &dyn GreenInterner) -> token::TokenKind {
        self.raw(db).kind
    }
    pub fn text(&self, db: &dyn GreenInterner) -> SmolStr {
        self.raw(db).text
    }
    pub fn width(&self, db: &dyn GreenInterner) -> u32 {
        self.raw(db).width()
    }
}
impl TypedSyntaxNode for Token {
    fn missing(db: &dyn GreenInterner) -> GreenId {
        db.intern_green(GreenNode::Token(token::Token::missing()))
    }
    fn from_syntax_node(db: &dyn GreenInterner, node: SyntaxNode) -> Self {
        let green = db.lookup_intern_green(node.0.green);
        match green {
            GreenNode::Internal(internal) => {
                panic!("Unexpected SyntaxKind {:?}. Expected a token.", internal.kind);
            }
            GreenNode::Token(_token) => Self { node },
        }
    }

    fn as_syntax_node(&self) -> SyntaxNode {
        self.node.clone()
    }
}

impl ast::Identifier {
    pub fn text(&self, db: &dyn GreenInterner) -> SmolStr {
        self.terminal(db).token(db).text(db)
    }
}
