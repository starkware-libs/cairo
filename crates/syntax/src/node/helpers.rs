use smol_str::SmolStr;

use super::ast;
use super::db::GreenInterner;

trait TerminalEx {
    fn text(&self, db: &dyn GreenInterner) -> SmolStr;
}

impl TerminalEx for ast::Terminal {
    fn text(&self, db: &dyn GreenInterner) -> SmolStr {
        self.token(db).text(db)
    }
}
