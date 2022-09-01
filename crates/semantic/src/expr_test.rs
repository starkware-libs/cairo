use assert_matches::assert_matches;
use defs::db::{AsDefsGroup, DefsDatabase, DefsGroup};
use filesystem::db::FilesDatabase;
use indoc::indoc;
use parser::db::ParserDatabase;
use parser::test_utils::prepare_test_expr;
use syntax::node::db::{AsSyntaxGroup, SyntaxDatabase, SyntaxGroup};

use super::compute_expr_semantic;
use crate::corelib::unit_ty;
use crate::db::{SemanticDatabase, SemanticGroup};
use crate::semantic;

#[salsa::database(SemanticDatabase, DefsDatabase, ParserDatabase, SyntaxDatabase, FilesDatabase)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl salsa::Database for DatabaseImpl {}
impl AsDefsGroup for DatabaseImpl {
    fn as_defs_group(&self) -> &(dyn DefsGroup + 'static) {
        self
    }
}
impl AsSyntaxGroup for DatabaseImpl {
    fn as_syntax_group(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}

#[test]
fn test_expr_literal() {
    let db_val = DatabaseImpl::default();
    let db = &db_val;
    let syntax = prepare_test_expr(db, "7");

    // Compute semantics of expr.
    let expr_id = compute_expr_semantic(db, syntax);
    let expr = db.lookup_intern_expr(expr_id);

    // Check expr.
    let semantic::ExprLiteral { value, ty } = match expr {
        crate::Expr::ExprLiteral(expr) => expr,
        _ => panic!("Expected a literal."),
    };
    assert_eq!(value, 7);
    assert_eq!(ty, db.core_felt_ty());
}

#[test]
fn test_expr_block() {
    let db_val = DatabaseImpl::default();
    let db = &db_val;
    let syntax = prepare_test_expr(
        db,
        indoc! {"
        {
            6;
            8;
        }
    "},
    );
    let db = &db_val;

    // Compute semantics of expr.
    let expr_id = compute_expr_semantic(db, syntax);
    let expr = db.lookup_intern_expr(expr_id);

    // Check expr.
    let statements = match expr {
        crate::Expr::ExprBlock(semantic::ExprBlock { statements, tail: None, ty }) => {
            assert_eq!(ty, unit_ty(db));
            statements
        }
        _ => panic!("Expected a block."),
    };
    match statements[..] {
        [stmt_id0, stmt_id1] => {
            let stmt0 = db.lookup_intern_statement(stmt_id0);
            let stmt1 = db.lookup_intern_statement(stmt_id1);
            assert_matches!(stmt0, semantic::Statement::Expr(_));
            assert_matches!(stmt1, semantic::Statement::Expr(_));
        }
        _ => panic!("Expected two statements."),
    }
}
