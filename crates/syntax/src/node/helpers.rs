use smol_str::SmolStr;

use super::db::SyntaxGroup;
use super::green::GreenNode;
use super::{ast, TokenGreen};
use crate::token::TokenKind;

pub trait TerminalEx {
    fn kind(&self, db: &dyn SyntaxGroup) -> TokenKind;
    fn text(&self, db: &dyn SyntaxGroup) -> SmolStr;
}

impl TerminalEx for ast::Terminal {
    fn kind(&self, db: &dyn SyntaxGroup) -> TokenKind {
        self.token(db).kind(db)
    }
    fn text(&self, db: &dyn SyntaxGroup) -> SmolStr {
        self.token(db).text(db)
    }
}

pub trait TerminalGreenEx {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr;
}
impl TerminalGreenEx for ast::TerminalGreen {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        match db.lookup_intern_green(self.0) {
            GreenNode::Internal(node) => TokenGreen(node.children[1]).text(db),
            GreenNode::Token(_) => panic!(),
        }
    }
}

pub trait ExprPathGreenEx {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr;
}
impl ExprPathGreenEx for ast::ExprPathGreen {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        match db.lookup_intern_green(self.0) {
            GreenNode::Internal(node) => {
                let last_even_index = (node.children.len() - 1) & (!1);
                ast::TerminalGreen(node.children[last_even_index]).identifier(db)
            }
            GreenNode::Token(_) => panic!(),
        }
    }
}
