#[cfg(test)]
#[path = "expr_test.rs"]
mod test;

use syntax::node::ast;

use crate::db::SemanticGroup;
use crate::{semantic, ExprId};

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
        ast::Expr::Block(_) => todo!(),
        ast::Expr::ExprMissing(_) => todo!(),
    };
    db.intern_expr(expr)
}
