use smol_str::SmolStr;
use utils::extract_matches;

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

/// Helper methods on TerminalGreen which are not auto-generated, and thus, do not appear
/// in ast.rs.
pub trait TerminalGreenEx {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr;
}
impl TerminalGreenEx for ast::TerminalGreen {
    /// Retrieves the text of the token.
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        let node = extract_matches!(
            db.lookup_intern_green(self.0),
            GreenNode::Internal,
            "Unexpected token"
        );
        TokenGreen(node.children[1]).text(db)
    }
}

pub trait ExprPathGreenEx {
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr;
}
impl ExprPathGreenEx for ast::ExprPathGreen {
    /// Retrieves the text of the last identifier in the path.
    fn identifier(&self, db: &dyn SyntaxGroup) -> SmolStr {
        let children = extract_matches!(
            db.lookup_intern_green(self.0),
            GreenNode::Internal,
            "Unexpected token"
        )
        .children;
        assert_eq!(children.len() & 1, 1, "Expected an odd number of elements in the path.");
        ast::TerminalGreen(*children.last().unwrap()).identifier(db)
    }
}
