use crate::db::SemanticGroup;
use crate::semantic;

#[cfg(test)]
mod test;

pub mod compute;
pub mod objects;

pub fn expr_semantic(db: &dyn SemanticGroup, item: semantic::ExprId) -> semantic::Expr {
    db.lookup_intern_expr(item)
}

pub fn statement_semantic(
    db: &dyn SemanticGroup,
    item: semantic::StatementId,
) -> semantic::Statement {
    db.lookup_intern_statement(item)
}
