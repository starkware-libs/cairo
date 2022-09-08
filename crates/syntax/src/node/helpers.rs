use smol_str::SmolStr;

use super::ast;
use super::db::SyntaxGroup;
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
