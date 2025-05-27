use cairo_lang_debug::debug::GetIdValue;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::FunctionWithBodyId;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_utils::Upcast;
use id_arena::Id;

use crate::db::{Elongate, SemanticGroup};
use crate::{Expr, Pattern, Statement};

/// Holds all the information needed for formatting expressions.
/// Acts like a "db" for DebugWithDb.
pub struct ExprFormatter<'db> {
    pub db: &'db dyn SemanticGroup,
    pub function_id: FunctionWithBodyId<'db>,
}

impl<'db> Upcast<'db, dyn SemanticGroup> for ExprFormatter<'db> {
    fn upcast(&'db self) -> &'db (dyn SemanticGroup) {
        self.db
    }
}
impl<'db> Upcast<'db, dyn DefsGroup> for ExprFormatter<'db> {
    fn upcast(&'db self) -> &'db (dyn DefsGroup) {
        self.db.upcast()
    }
}

impl<'db> Upcast<'db, dyn ParserGroup> for ExprFormatter<'db> {
    fn upcast(&'db self) -> &'db (dyn ParserGroup) {
        self.db.upcast()
    }
}

impl<'db> GetIdValue<'db, ExprFormatter<'db>, Pattern<'db>> for ExprFormatter<'db> {
    fn get_id_value(&self, id: Id<Pattern<'db>>) -> Pattern<'db> {
        self.db.pattern_semantic(self.function_id, id)
    }
}

impl<'db> GetIdValue<'db, ExprFormatter<'db>, Expr<'db>> for ExprFormatter<'db> {
    fn get_id_value(&self, id: Id<Expr<'db>>) -> Expr<'db> {
        self.db.expr_semantic(self.function_id, id)
    }
}

impl<'db> GetIdValue<'db, ExprFormatter<'db>, Statement<'db>> for ExprFormatter<'db> {
    fn get_id_value(&self, id: Id<Statement<'db>>) -> Statement<'db> {
        self.db.statement_semantic(self.function_id, id)
    }
}

/// A wrapper around std::fmt::Formatter that counts the number of characters written so far.
pub struct CountingWriter<'a, 'b> {
    inner: &'a mut std::fmt::Formatter<'b>,
    count: usize,
}

impl<'a, 'b> CountingWriter<'a, 'b> {
    pub fn new(inner: &'a mut std::fmt::Formatter<'b>) -> Self {
        Self { inner, count: 0 }
    }

    pub fn count(&self) -> usize {
        self.count
    }
}

impl<'a, 'b> std::fmt::Write for CountingWriter<'a, 'b> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.count += s.len();
        self.inner.write_str(s)
    }
}
