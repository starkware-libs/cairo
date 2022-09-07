use assert_matches::assert_matches;
use debug::DebugWithDb;
use defs::db::{AsDefsGroup, DefsDatabase, DefsGroup};
use defs::ids::{LanguageElementId, ModuleId, ModuleItemId, VarId};
use filesystem::db::{AsFilesGroup, FilesDatabase, FilesGroup};
use indoc::indoc;
use parser::db::ParserDatabase;
use pretty_assertions::assert_eq;
use smol_str::SmolStr;
use syntax::node::db::{AsSyntaxGroup, SyntaxDatabase, SyntaxGroup};

use crate::corelib::{core_felt_ty, unit_ty};
use crate::db::{SemanticDatabase, SemanticGroup};
use crate::test_utils::{setup_test_expr, setup_test_function};
use crate::{semantic, ExprId, StatementId, TypeId};

#[salsa::database(SemanticDatabase, DefsDatabase, ParserDatabase, SyntaxDatabase, FilesDatabase)]
#[derive(Default)]
pub struct DatabaseForTesting {
    storage: salsa::Storage<DatabaseForTesting>,
}
impl salsa::Database for DatabaseForTesting {}
impl AsFilesGroup for DatabaseForTesting {
    fn as_files_group(&self) -> &(dyn FilesGroup + 'static) {
        self
    }
}
impl AsSyntaxGroup for DatabaseForTesting {
    fn as_syntax_group(&self) -> &(dyn SyntaxGroup + 'static) {
        self
    }
}
impl AsDefsGroup for DatabaseForTesting {
    fn as_defs_group(&self) -> &(dyn DefsGroup + 'static) {
        self
    }
}

#[test]
fn test_expr_literal() {
    let mut db_val = DatabaseForTesting::default();
    let (_module_id, expr_id) = setup_test_expr(&mut db_val, "7", "", "").expect("");
    let db = &db_val;
    let expr = db.lookup_intern_expr(expr_id);
    // TODO(spapini): Currently, DebugWithDb can't "switch" dbs, and thus ExternTypeId is not
    // followed (it uses SyntaxGroup, and not SemanticGroup).
    // Fix this.
    assert_eq!(
        format!("{:?}", expr.debug(db)),
        "ExprLiteral(ExprLiteral { value: 7, ty: Concrete(ExternTypeId(core::felt)) })"
    );

    // Check expr.
    let semantic::ExprLiteral { value, ty } = match expr {
        crate::Expr::ExprLiteral(expr) => expr,
        _ => panic!("Expected a literal."),
    };
    assert_eq!(value, 7);
    assert_eq!(ty, db.core_felt_ty());
}

#[test]
fn test_expr_operator() {
    let mut db_val = DatabaseForTesting::default();
    let (_module_id, expr_id) = setup_test_expr(&mut db_val, "5 + 9 * 3", "", "").expect("");
    let db = &db_val;
    let expr = db.lookup_intern_expr(expr_id);
    // TODO(spapini): Make transparent DebugWithDb attribute, to have better outputs.
    // TODO(spapini): Have better whitespaces here somehow.
    assert_eq!(
        format!("{:?}", expr.debug(db)),
        "ExprFunctionCall(ExprFunctionCall { function: \
         Concrete(ExternFunctionId(core::felt_add)), args: [ExprLiteral(ExprLiteral { value: 5, \
         ty: Concrete(ExternTypeId(core::felt)) }), ExprFunctionCall(ExprFunctionCall { function: \
         Concrete(ExternFunctionId(core::felt_mul)), args: [ExprLiteral(ExprLiteral { value: 9, \
         ty: Concrete(ExternTypeId(core::felt)) }), ExprLiteral(ExprLiteral { value: 3, ty: \
         Concrete(ExternTypeId(core::felt)) })], ty: Concrete(ExternTypeId(core::felt)) })], ty: \
         Concrete(ExternTypeId(core::felt)) })"
    );
}

// TODO(yuval): split test utils and move this test to db_test/type_test.
#[test]
fn test_function_with_param() {
    let mut db_val = DatabaseForTesting::default();
    let (_module_id, function) =
        setup_test_function(&mut db_val, "func foo(a: felt) {}", "foo", "").expect("");
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
    let mut db_val = DatabaseForTesting::default();
    let (_module_id, function) =
        setup_test_function(&mut db_val, "func foo() -> felt {}", "foo", "").expect("");
    let _db = &db_val;
    let signature = function.signature;

    // TODO(spapini): Verify params names and tests after StablePtr feature is added.
    let _ret_ty = signature.return_type;
}

#[test]
fn test_let_statement() {
    let mut db_val = DatabaseForTesting::default();
    let (module_id, function) = setup_test_function(
        &mut db_val,
        indoc! {"
            func foo() {
                let a: felt = 3;
                let b = a;
            }
        "},
        "foo",
        "",
    )
    .expect("");
    let db = &db_val;

    let _signature = function.signature;

    // TODO(spapini): Verify params names and tests after StablePtr feature is added.
    let statements = match db.lookup_intern_expr(function.body) {
        crate::Expr::ExprBlock(block) => {
            assert!(block.tail.is_none());
            block.statements
        }
        _ => panic!(),
    };

    // Verify the statements
    assert_eq!(statements.len(), 2);
    assert_let_statement_with_literal(db, statements[0], module_id, "a".into(), 3);
    assert_let_statement_with_var(
        db,
        statements[1],
        module_id,
        "b".into(),
        "a".into(),
        core_felt_ty(db),
    );
}

#[test]
fn test_expr_var() {
    let mut db_val = DatabaseForTesting::default();
    let (_module_id, function) = setup_test_function(
        &mut db_val,
        indoc! {"
            func foo(a: felt) {
                a
            }
        "},
        "foo",
        "",
    )
    .expect("");
    let db = &db_val;

    let expr_id = match db.lookup_intern_expr(function.body) {
        crate::Expr::ExprBlock(block) => block.tail.unwrap(),
        _ => panic!(),
    };

    // Check expr.
    let semantic::ExprVar { var: _, ty: _ } = match db.lookup_intern_expr(expr_id) {
        crate::Expr::ExprVar(expr) => expr,
        _ => panic!("Expected a variable."),
    };
    // TODO(spapini): Check Var against param using param.id.
}

#[test]
fn test_expr_match() {
    let mut db_val = DatabaseForTesting::default();
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
    )
    .expect("");
    let db = &db_val;
    let tail_expr_id = match db.lookup_intern_expr(func.body) {
        crate::Expr::ExprBlock(block) => block.tail.unwrap(),
        _ => panic!(),
    };
    let expr = match db.lookup_intern_expr(tail_expr_id) {
        crate::Expr::ExprMatch(expr) => expr,
        _ => panic!(),
    };
    assert_eq!(expr.arms.len(), 2);
    // TODO(spapini): Test the rest, possibly using DebugWithDb.
}

#[test]
fn test_expr_block() {
    let mut db_val = DatabaseForTesting::default();
    let (_module_id, expr_id) = setup_test_expr(&mut db_val, "{6;8;}", "", "").expect("");
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
    let mut db_val = DatabaseForTesting::default();
    let (_module_id, expr_id) = setup_test_expr(&mut db_val, "{6;8;9}", "", "").expect("");
    let db = &db_val;
    let expr = db.lookup_intern_expr(expr_id);

    // Check expr.
    let (statements, tail) = match expr {
        crate::Expr::ExprBlock(semantic::ExprBlock { statements, tail: Some(tail), ty }) => {
            assert_eq!(ty, core_felt_ty(db));
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
    let mut db_val = DatabaseForTesting::default();
    // TODO(spapini): Add types.
    let (_module_id, expr_id) =
        setup_test_expr(&mut db_val, "foo()", "func foo() {6;}", "").expect("");
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
fn test_expr_call_missing() {
    let mut db_val = DatabaseForTesting::default();
    // TODO(spapini): Add types.
    let res = setup_test_expr(&mut db_val, "foo()", "", "");
    let db = &db_val;

    // Check expr.
    assert_eq!(
        res.diagnostics.format(db),
        indoc! { "
            error: Unknown function
             --> test.cairo:2:1
            foo()
            ^*^

        "}
    );
    assert_eq!(format!("{:?}", res.value.0.debug(db)), "ModuleId(test_crate)");
    assert_eq!(
        format!("{:?}", res.value.1.debug(db)),
        "ExprFunctionCall(ExprFunctionCall { function: Missing, args: [], ty: Missing })"
    );
}

#[test]
fn test_function_body() {
    let mut db_val = DatabaseForTesting::default();
    let (module_id, _module_syntax) = setup_test_function(
        &mut db_val,
        indoc! {"
            func foo(a: felt) {
                a;
            }
        "},
        "foo",
        "",
    )
    .expect("");
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

pub fn assert_let_statement_with_literal(
    db: &dyn SemanticGroup,
    statement_id: StatementId,
    module_id: ModuleId,
    var_name: SmolStr,
    literal_value: usize,
) {
    let rhs = assert_let_statement_lhs_and_get_rhs(db, statement_id, module_id, var_name);
    if let semantic::Expr::ExprLiteral(literal) = db.lookup_intern_expr(rhs) {
        assert_eq!(literal.value, literal_value);
        assert_eq!(literal.ty, core_felt_ty(db));
    } else {
        panic!("Expected a literal expression");
    }
}

pub fn assert_let_statement_with_var(
    db: &dyn SemanticGroup,
    statement_id: StatementId,
    module_id: ModuleId,
    var_name: SmolStr,
    expr_var_name: SmolStr,
    expr_var_type: TypeId,
) {
    let rhs = assert_let_statement_lhs_and_get_rhs(db, statement_id, module_id, var_name);
    if let semantic::Expr::ExprVar(var) = db.lookup_intern_expr(rhs) {
        assert_eq!(var.var.name(db.as_defs_group()), expr_var_name);
        assert_eq!(var.ty, expr_var_type);
    } else {
        panic!("Expected a var expression");
    }
}

fn assert_let_statement_lhs_and_get_rhs(
    db: &dyn SemanticGroup,
    statement_id: StatementId,
    module_id: ModuleId,
    var_name: SmolStr,
) -> ExprId {
    let stmt = db.lookup_intern_statement(statement_id);
    let let_stmt = if let semantic::Statement::Let(let_stmt) = stmt {
        let_stmt
    } else {
        panic!("Expected a let statement")
    };
    assert_eq!(let_stmt.var.id.module(db.as_defs_group()), module_id);
    assert_eq!(let_stmt.var.id.name(db.as_defs_group()), var_name);
    assert_eq!(let_stmt.var.ty, core_felt_ty(db));

    let_stmt.expr
}
