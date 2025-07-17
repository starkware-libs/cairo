use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::FunctionWithBodyId;
use cairo_lang_utils::Upcast;

use crate::db::SemanticGroup;

/// Holds all the information needed for formatting expressions.
/// Acts like a "db" for DebugWithDb.
pub struct ExprFormatter<'a> {
    pub db: &'a (dyn SemanticGroup + 'static),
    pub function_id: FunctionWithBodyId,
}

impl Upcast<dyn SemanticGroup + 'static> for ExprFormatter<'_> {
    fn upcast(&self) -> &(dyn SemanticGroup + 'static) {
        self.db
    }
}
impl Upcast<dyn DefsGroup + 'static> for ExprFormatter<'_> {
    fn upcast(&self) -> &(dyn DefsGroup + 'static) {
        self.db
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
