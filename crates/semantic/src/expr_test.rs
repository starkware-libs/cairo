use assert_matches::assert_matches;
use defs::db::{AsDefsGroup, DefsDatabase, DefsGroup};
use defs::ids::{HasName, ModuleItemId, VarId};
use filesystem::db::{AsFilesGroup, FilesDatabase, FilesGroup};
use indoc::indoc;
use parser::db::ParserDatabase;
use syntax::node::db::{AsSyntaxGroup, SyntaxDatabase, SyntaxGroup};

use crate::corelib::unit_ty;
use crate::db::{SemanticDatabase, SemanticGroup};
use crate::semantic;
use crate::test_utils::{setup_test_expr, setup_test_function};

#[salsa::database(SemanticDatabase, DefsDatabase, ParserDatabase, SyntaxDatabase, FilesDatabase)]
#[derive(Default)]
pub struct DatabaseImpl {
    storage: salsa::Storage<DatabaseImpl>,
}
impl salsa::Database for DatabaseImpl {}
impl AsFilesGroup for DatabaseImpl {
    fn as_files_group(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}
impl AsSyntaxGroup for DatabaseImpl {
    fn as_syntax_group(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}
impl AsDefsGroup for DatabaseImpl {
    fn as_defs_group(&self) -> &(dyn DefsGroup + 'static) {
        self
    }
}

#[test]
fn test_expr_literal() {
    let mut db_val = DatabaseImpl::default();
    let (_module_id, expr_id) = setup_test_expr(&mut db_val, "7", "", "");
    let db = &db_val;
    let expr = db.lookup_intern_expr(expr_id);

    // Check expr.
    let semantic::ExprLiteral { value, ty } = match expr {
        crate::Expr::ExprLiteral(expr) => expr,
        _ => panic!("Expected a literal."),
    };
    assert_eq!(value, 7);
    assert_eq!(ty, db.core_felt_ty());
}

// TODO(yuval): split test utils and move this test to db_test/type_test.
#[test]
fn test_function_with_param() {
    let mut db_val = DatabaseImpl::default();
    let (_module_id, function) =
        setup_test_function(&mut db_val, "func foo(a: felt) {}", "foo", "");
    let _db = &db_val;
    let signature = function.signature;

    // TODO(spapini): Verify params names and tests after StablePtr feature is added.
    assert_eq!(signature.params.len(), 1);
    let param = &signature.params[0];
    let _param_ty = param.ty;
}

// TODO(yuval): split test utils and move this test to db_test/type_test.
#[test]
fn test_function_with_return_type() {
    let mut db_val = DatabaseImpl::default();
    let (_module_id, function) =
        setup_test_function(&mut db_val, "func foo() -> felt {}", "foo", "");
    let _db = &db_val;
    let signature = function.signature;

    // TODO(spapini): Verify params names and tests after StablePtr feature is added.
    let _ret_ty = signature.return_type;
}

#[test]
fn test_expr_var() {
    let mut db_val = DatabaseImpl::default();
    let (_module_id, function) = setup_test_function(
        &mut db_val,
        indoc! {"
            func foo(a: felt) {
                a
            }
        "},
        "foo",
        "",
    );
    let db = &db_val;

    let expr = match db.lookup_intern_expr(function.body) {
        crate::Expr::ExprBlock(block) => block.tail.unwrap(),
        _ => panic!(),
    };

    // Check expr.
    let semantic::ExprVar { var: _, ty: _ } = match db.lookup_intern_expr(expr) {
        crate::Expr::ExprVar(expr) => expr,
        _ => panic!("Expected a variable."),
    };
    // TODO(spapini): Check Var against param using param.id.
}

#[test]
fn test_expr_match() {
    let mut db_val = DatabaseImpl::default();
    let (_module_id, func) = setup_test_function(
        &mut db_val,
        indoc! {"
            func foo(a: felt) {
                match a {
                    0 => 0,
                    _ => 1,
                }
            }
        "},
        "foo",
        "",
    );
    let db = &db_val;
    let expr_id = match db.lookup_intern_expr(func.body) {
        crate::Expr::ExprBlock(block) => block.tail.unwrap(),
        _ => panic!(),
    };
    let expr = match db.lookup_intern_expr(expr_id) {
        crate::Expr::ExprMatch(expr) => expr,
        _ => panic!(),
    };
    assert_eq!(expr.arms.len(), 2);
    // TODO(spapini): Test the rest, possibly using DebugWithDb.
}

#[test]
fn test_expr_block() {
    let mut db_val = DatabaseImpl::default();
    let (_module_id, expr_id) = setup_test_expr(&mut db_val, "{6;8;}", "", "");
    let db = &db_val;
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

#[test]
fn test_expr_block_with_tail_expression() {
    let mut db_val = DatabaseImpl::default();
    let (_module_id, expr_id) = setup_test_expr(&mut db_val, "{6;8;9}", "", "");
    let db = &db_val;
    let expr = db.lookup_intern_expr(expr_id);

    // Check expr.
    let (statements, tail) = match expr {
        crate::Expr::ExprBlock(semantic::ExprBlock { statements, tail: Some(tail), ty }) => {
            assert_eq!(ty, unit_ty(db));
            (statements, tail)
        }
        _ => panic!("Expected a block."),
    };
    // Check tail expression.
    match db.lookup_intern_expr(tail) {
        semantic::Expr::ExprLiteral(expr_literal) => {
            assert_eq!(expr_literal.value, 9)
        }
        _ => panic!("Expected a literal expression."),
    }
    // Check statements.
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

#[test]
fn test_expr_call() {
    let mut db_val = DatabaseImpl::default();
    // TODO(spapini): Add types.
    let (_module_id, expr_id) = setup_test_expr(&mut db_val, "foo()", "func foo() {6;}", "");
    let db = &db_val;
    let expr = db.lookup_intern_expr(expr_id);

    // Check expr.
    match expr {
        semantic::Expr::ExprFunctionCall(semantic::ExprFunctionCall { function: _, args, ty }) => {
            assert!(args.is_empty());
            assert_eq!(ty, unit_ty(db));
        }
        _ => panic!("Unexpected expr"),
    }
}

#[test]
fn test_function_body() {
    let mut db_val = DatabaseImpl::default();
    let (module_id, _module_syntax) = setup_test_function(
        &mut db_val,
        indoc! {"
            func foo(a: felt) {
                a;
            }
        "},
        "foo",
        "",
    );
    let db = &db_val;
    let item_id =
        db.module_item_by_name(module_id, "foo".into()).expect("Unexpected diagnostics").unwrap();
    let function_id =
        if let ModuleItemId::FreeFunction(function_id) = item_id { function_id } else { panic!() };
    let function = db.free_function_semantic(function_id).expect("Unexpected diagnostics").unwrap();

    // Test the resulting semantic function body.
    let expr = match db.lookup_intern_expr(function.body) {
        crate::Expr::ExprBlock(expr) => expr,
        _ => panic!(),
    };
    assert_eq!(expr.statements.len(), 1);
    let expr = db.lookup_intern_expr(match db.lookup_intern_statement(expr.statements[0]) {
        crate::Statement::Expr(expr) => expr,
        _ => panic!(),
    });
    let param = match expr {
        crate::Expr::ExprVar(semantic::ExprVar { var: VarId::Param(param_id), ty: _ }) => param_id,
        _ => panic!(),
    };
    assert_eq!(param.name(db), "a");
}
