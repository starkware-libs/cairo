use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use assert_matches::assert_matches;
use defs::db::{AsDefsGroup, DefsDatabase, DefsGroup};
use defs::ids::{FreeFunctionLongId, GenericFunctionId, ParamLongId, VarId};
use filesystem::db::{AsFilesGroup, FilesDatabase, FilesGroup};
use indoc::indoc;
use parser::db::ParserDatabase;
use syntax::node::ast;
use syntax::node::db::{AsSyntaxGroup, SyntaxDatabase, SyntaxGroup};

use super::compute_expr_semantic;
use crate::corelib::{core_felt_ty, unit_ty};
use crate::db::{SemanticDatabase, SemanticGroup};
use crate::expr::{ComputationContext, Environment};
use crate::semantic;
use crate::test_utils::setup_test_module;

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
    let (module_id, module_syntax) = setup_test_module(&mut db_val, "func foo() { 7 }");
    let db = &db_val;
    // TODO(spapini): When a tail expression is supported, take the syntax from the tail instead
    // of the statements.
    let syntax = match &extract_function_body(db, module_syntax, 0).statements(db).elements(db)[0] {
        ast::Statement::Expr(syntax) => syntax.expr(db),
        _ => panic!("Expected an expression statement"),
    };

    // Compute semantics of expr.
    let mut ctx = ComputationContext {
        db,
        module_id,
        environment: Rc::new(Environment { parent: None, variables: HashMap::new() }),
    };
    let expr_id = compute_expr_semantic(&mut ctx, syntax);
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
    let (module_id, module_syntax) = setup_test_module(
        &mut db_val,
        indoc! {"
            func foo(a: felt) {}
        "},
    );
    let db = &db_val;
    // TODO(spapini): When a tail expression in a block is supported, take the syntax from the tail
    // instead of from the statements.
    assert!(extract_function_body(db, module_syntax, 0).statements(db).elements(db).is_empty());

    // Prepare expectations
    let free_function_id =
        db.intern_free_function(FreeFunctionLongId { parent: module_id, name: "foo".into() });
    let felt_id = core_felt_ty(db);

    // Compute semantics of signature.
    let signature = db
        .generic_function_signature_semantic(GenericFunctionId::Free(free_function_id))
        .expect("Unexpected diagnostic")
        .unwrap();

    // Verify the parameter name and type.
    assert_eq!(signature.params.len(), 1);
    assert_eq!(signature.params[0].name, "a");
    assert_eq!(signature.params[0].ty, felt_id);
}

// TODO(yuval): split test utils and move this test to db_test/type_test.
#[test]
fn test_function_with_return_type() {
    let mut db_val = DatabaseImpl::default();
    let (module_id, module_syntax) = setup_test_module(
        &mut db_val,
        indoc! {"
            func foo() -> felt {}
        "},
    );
    let db = &db_val;
    // TODO(spapini): When a tail expression in a block is supported, take the syntax from the tail
    // instead of from the statements.
    assert!(extract_function_body(db, module_syntax, 0).statements(db).elements(db).is_empty());

    // Prepare expectations
    let free_function_id =
        db.intern_free_function(FreeFunctionLongId { parent: module_id, name: "foo".into() });
    let felt_id = core_felt_ty(db);

    // Compute semantics of signature.
    let signature = db
        .generic_function_signature_semantic(GenericFunctionId::Free(free_function_id))
        .expect("Unexpected diagnostic")
        .unwrap();

    // Verify the return type.
    assert_eq!(signature.params.len(), 0);
    assert_eq!(signature.return_type, felt_id);
}

#[test]
fn test_expr_var() {
    let mut db_val = DatabaseImpl::default();
    let (module_id, module_syntax) = setup_test_module(
        &mut db_val,
        indoc! {"
            func foo(a: felt) {
                a
            }
        "},
    );
    let db = &db_val;
    // TODO(spapini): When a tail expression in a block is supported, take the syntax from the tail
    // instead of from the statements.
    let syntax = match &extract_function_body(db, module_syntax, 0).statements(db).elements(db)[0] {
        ast::Statement::Expr(syntax) => syntax.expr(db),
        _ => panic!("Expected an expression statement"),
    };

    // Compute semantics of signature.
    let free_function_id =
        db.intern_free_function(FreeFunctionLongId { parent: module_id, name: "foo".into() });
    let signature = db
        .generic_function_signature_semantic(GenericFunctionId::Free(free_function_id))
        .expect("Unexpected diagnostic")
        .unwrap();

    // Compute semantics of expr.
    let var_id = VarId::Param(db.intern_param(ParamLongId {
        parent: defs::ids::ParamContainerId::FreeFunction(free_function_id),
        name: signature.params[0].name.clone(),
    }));
    let mut ctx = ComputationContext {
        db,
        module_id,
        environment: Rc::new(Environment {
            parent: None,
            variables: [("a".into(), var_id)].into_iter().collect(),
        }),
    };
    let expr_id = compute_expr_semantic(&mut ctx, syntax);
    let expr = db.lookup_intern_expr(expr_id);

    // Check expr.
    let var = match expr {
        crate::Expr::ExprVar(expr) => expr.var,
        _ => panic!("Expected a variable."),
    };
    assert_eq!(var, var_id);
    // TODO(spapini): Check type.
}

#[test]
fn test_expr_match() {
    let mut db_val = DatabaseImpl::default();
    let (module_id, module_syntax) = setup_test_module(
        &mut db_val,
        indoc! {"
            extern type felt;
            func foo(a: felt) {
                match a {
                    0 => 0,
                    _ => 1,
                }
            }
        "},
    );
    let db = &db_val;
    // TODO(spapini): When a tail expression in a block is supported, take the syntax from the tail
    // instead of from the statements.
    let expr_syntax =
        match &extract_function_body(db, module_syntax, 1).statements(db).elements(db)[0] {
            ast::Statement::Expr(syntax) => syntax.expr(db),
            _ => panic!("Expected an expression statement"),
        };
    let _match_syntax = match expr_syntax {
        ast::Expr::Match(match_syntax) => match_syntax.expr(db_val.as_syntax_group()),
        _ => panic!("Expected a match statement"),
    };

    // Compute semantics of signature.
    let free_function_id =
        db.intern_free_function(FreeFunctionLongId { parent: module_id, name: "foo".into() });
    let _signature = db
        .generic_function_signature_semantic(GenericFunctionId::Free(free_function_id))
        .expect("Unexpected diagnostic")
        .unwrap();

    // TODO(yuval/spapini): complete test with test utils.
}

#[test]
fn test_expr_block() {
    let mut db_val = DatabaseImpl::default();
    let (module_id, module_syntax) = setup_test_module(
        &mut db_val,
        indoc! {"
            func foo() {
                6;
                8;
            }
        "},
    );
    let db = &db_val;
    let syntax = ast::Expr::Block(extract_function_body(db, module_syntax, 0));

    // Compute semantics of expr.
    let mut ctx = ComputationContext {
        db,
        module_id,
        environment: Rc::new(Environment { parent: None, variables: HashMap::new() }),
    };
    let expr_id = compute_expr_semantic(&mut ctx, syntax);
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
    let (module_id, module_syntax) = setup_test_module(
        &mut db_val,
        indoc! {"
            func foo() {
                6;
                8;
                9
            }
        "},
    );
    let db = &db_val;
    let syntax = ast::Expr::Block(extract_function_body(db, module_syntax, 0));

    // Compute semantics of expr.
    let mut ctx = ComputationContext {
        db,
        module_id,
        environment: Rc::new(Environment { parent: None, variables: HashMap::new() }),
    };
    let expr_id = compute_expr_semantic(&mut ctx, syntax);
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
    let (module_id, module_syntax) = setup_test_module(
        &mut db_val,
        indoc! {"
            func foo() {
                bar();
            }
            func bar() {
                6;
            }
        "},
    );
    let db = &db_val;
    // TODO(spapini): When a tail expression in a block is supported, take the syntax from the tail
    // instead of from the statements.
    let syntax = match &extract_function_body(db, module_syntax, 0).statements(db).elements(db)[0] {
        ast::Statement::Expr(syntax) => syntax.expr(db),
        _ => panic!("Expected an expression statement"),
    };

    // Compute semantics of expr.
    let mut ctx = ComputationContext {
        db,
        module_id,
        environment: Rc::new(Environment { parent: None, variables: HashMap::new() }),
    };
    let expr_id = compute_expr_semantic(&mut ctx, syntax);
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
    let (module_id, _module_syntax) = setup_test_module(
        &mut db_val,
        indoc! {"
            func foo(a: felt) {
                a;
            }
        "},
    );
    let db = &db_val;
    let function_id =
        db.intern_free_function(FreeFunctionLongId { parent: module_id, name: "foo".into() });
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
    let param = db.lookup_intern_param(match expr {
        crate::Expr::ExprVar(semantic::ExprVar { var: VarId::Param(param_id), ty: _ }) => param_id,
        _ => panic!(),
    });
    assert_eq!(param.name, "a");
}

fn extract_function_body(
    db: &dyn SyntaxGroup,
    module_syntax: Arc<ast::SyntaxFile>,
    index: usize,
) -> ast::ExprBlock {
    let syntax = &module_syntax.items(db).elements(db)[index];
    let function_syntax = match syntax {
        ast::Item::Function(function_syntax) => function_syntax,
        variant => panic!("Not a free function, actual: {variant:?}"),
    };
    function_syntax.body(db)
}
