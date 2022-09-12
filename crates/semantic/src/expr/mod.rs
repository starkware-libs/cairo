//! This module is responsible for inner code elements, such as expressions and statements.

use crate::db::SemanticGroup;
use crate::semantic;

#[cfg(test)]
mod test;

pub mod compute;
pub mod objects;

/// Query implementation of [crate::db::SemanticGroup::expr_semantic].
pub fn expr_semantic(db: &dyn SemanticGroup, item: semantic::ExprId) -> semantic::Expr {
    db.lookup_intern_expr(item)
}

/// Query implementation of [crate::db::SemanticGroup::statement_semantic].
pub fn statement_semantic(
    db: &dyn SemanticGroup,
    item: semantic::StatementId,
) -> semantic::Statement {
    db.lookup_intern_statement(item)
}
