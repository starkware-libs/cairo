use smol_str::SmolStr;

use super::ast;
use super::db::SyntaxGroup;

trait TerminalEx {
    fn text(&self, db: &dyn SyntaxGroup) -> SmolStr;
}

impl TerminalEx for ast::Terminal {
    fn text(&self, db: &dyn SyntaxGroup) -> SmolStr {
        self.token(db).text(db)
    }
}
