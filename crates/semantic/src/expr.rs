#[cfg(test)]
#[path = "expr_test.rs"]
mod test;

use syntax::node::ast;

use crate::corelib::unit_ty;
use crate::db::SemanticGroup;
use crate::{semantic, ExprId, StatementId};

pub fn compute_expr_semantic(db: &dyn SemanticGroup, syntax: ast::Expr) -> ExprId {
    let syntax_db = db.as_syntax_group();
    // TODO: When semantic::Expr holds the syntax pointer, add it here as well.
    let expr = match syntax {
        ast::Expr::Path(_) => todo!(),
        ast::Expr::Literal(literal_syntax) => {
            // TODO(spapini): Use TerminalEx.
            let text = literal_syntax.terminal(syntax_db).token(syntax_db).text(syntax_db);
            // TODO(spapini): Diagnostics.
            let value = text.parse::<usize>().unwrap();
            let ty = db.core_felt_ty();
            semantic::Expr::ExprLiteral(semantic::ExprLiteral { value, ty })
        }
        ast::Expr::Parenthesized(_) => todo!(),
        ast::Expr::Unary(_) => todo!(),
        ast::Expr::Binary(_) => todo!(),
        ast::Expr::Tuple(_) => todo!(),
        ast::Expr::FunctionCall(_) => todo!(),
        ast::Expr::StructCtorCall(_) => todo!(),
        ast::Expr::Block(block_syntax) => semantic::Expr::ExprBlock(semantic::ExprBlock {
            statements: block_syntax
                .statements(syntax_db)
                .elements(syntax_db)
                .into_iter()
                .map(|statement_syntax| compute_statement_semantic(db, statement_syntax))
                .collect(),
            // TODO(spapini): Handle tail when it exists in ast.
            tail: None,
            ty: unit_ty(db),
        }),
        ast::Expr::ExprMissing(_) => todo!(),
    };
    db.intern_expr(expr)
}

pub fn compute_statement_semantic(db: &dyn SemanticGroup, syntax: ast::Statement) -> StatementId {
    let syntax_db = db.as_syntax_group();
    let statement = match syntax {
        ast::Statement::Let(_) => todo!(),
        ast::Statement::Expr(expr_syntax) => {
            semantic::Statement::Expr(compute_expr_semantic(db, expr_syntax.expr(syntax_db)))
        }
        ast::Statement::Return(_) => todo!(),
        ast::Statement::StatementMissing(_) => todo!(),
    };
    db.intern_statement(statement)
}
