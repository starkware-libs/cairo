use cairo_lang_debug::DebugWithDb;
use cairo_lang_debug::debug::DebugDbUpcast;
use cairo_lang_defs::ids::FunctionWithBodyId;
use salsa::Database;

use crate::items::function_with_body::FunctionWithBodySemantic;
use crate::{ExprId, PatternId, StatementId};

/// Holds all the information needed for formatting expressions.
/// Acts like a "db" for DebugWithDb.
pub struct ExprFormatter<'db> {
    pub db: &'db dyn Database,
    pub function_id: FunctionWithBodyId<'db>,
}

impl<'db> DebugDbUpcast<'db, dyn Database> for ExprFormatter<'db> {
    fn debug_db_upcast(&'db self) -> &'db dyn Database {
        self.db
    }
}

impl<'db> DebugWithDb<'db> for ExprId {
    type Db = ExprFormatter<'db>;
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        let expr = db.db.expr_semantic(db.function_id, *self);
        expr.fmt(f, db)
    }
}

impl<'db> DebugWithDb<'db> for PatternId {
    type Db = ExprFormatter<'db>;
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        let pattern = db.db.pattern_semantic(db.function_id, *self);
        pattern.fmt(f, db)
    }
}

impl<'db> DebugWithDb<'db> for StatementId {
    type Db = ExprFormatter<'db>;
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        let statement = db.db.statement_semantic(db.function_id, *self);
        statement.fmt(f, db)
    }
}
