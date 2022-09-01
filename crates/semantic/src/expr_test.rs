use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use assert_matches::assert_matches;
use defs::db::{AsDefsGroup, DefsDatabase, DefsGroup};
use defs::ids::{FreeFunctionLongId, VarId};
use filesystem::db::{FilesDatabase, FilesGroup, ProjectConfig};
use filesystem::ids::{CrateLongId, FileLongId, ModuleId, VirtualFile};
use indoc::indoc;
use parser::db::{ParserDatabase, ParserGroup};
use syntax::node::ast::{self, SyntaxFile};
use syntax::node::db::{AsSyntaxGroup, SyntaxDatabase, SyntaxGroup};

use super::compute_expr_semantic;
use crate::corelib::unit_ty;
use crate::db::{SemanticDatabase, SemanticGroup};
use crate::expr::{ComputationContext, Environment};
use crate::{semantic, GenericFunctionId};

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
    let mut db_val = DatabaseImpl::default();
    let (module_id, module_syntax) = setup_test_module(&mut db_val, "func foo() { 7 }");
    let db = &db_val;
    // TODO(spapini): When a tail expression is supported, take the syntax from the tail instead
    // of the statements.
    let syntax = match &extract_function_body(db, module_syntax, 0).statements(db).elements(db)[0] {
        ast::Statement::Expr(syntax) => syntax.expr(db),
        _ => panic!("Expected an expression statemnet"),
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

#[test]
fn test_expr_var() {
    let mut db_val = DatabaseImpl::default();
    let (module_id, module_syntax) = setup_test_module(&mut db_val, "func foo(a) { a }");
    let db = &db_val;
    // TODO(spapini): When a tail expression in a block is supported, take the syntax from the tail
    // instead of from the statements.
    let syntax = match &extract_function_body(db, module_syntax, 0).statements(db).elements(db)[0] {
        ast::Statement::Expr(syntax) => syntax.expr(db),
        _ => panic!("Expected an expression statement"),
    };

    // Compute semantics of signature.
    let signature = db
        .generic_function_signature_semantic(GenericFunctionId::Free(
            db.intern_free_function(FreeFunctionLongId { parent: module_id, name: "foo".into() }),
        ))
        .expect("Unexpected diagnostic")
        .unwrap();

    // Compute semantics of expr.
    let var_id = VarId::Param(signature.params[0]);
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
    let semantic::ExprVar { var, ty: _ } = match expr {
        crate::Expr::ExprVar(expr) => expr,
        _ => panic!("Expected a variable."),
    };
    assert_eq!(var, var_id);
    // TODO(spapini): Check type.
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

fn setup_test_module(db: &mut DatabaseImpl, content: &str) -> (ModuleId, Arc<SyntaxFile>) {
    let crate_id = db.intern_crate(CrateLongId("test_crate".into()));
    let file_id = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "test.cairo".into(),
        content: Arc::new(content.to_string()),
    }));
    db.set_project_config(ProjectConfig {
        crate_roots: [(crate_id, file_id)].into_iter().collect(),
    });
    let module_id = ModuleId::CrateRoot(crate_id);
    let module_syntax = db.file_syntax(db.module_file(module_id).unwrap()).expect("").unwrap();
    (module_id, module_syntax)
}

fn extract_function_body(
    db: &dyn SyntaxGroup,
    module_syntax: Arc<ast::SyntaxFile>,
    index: usize,
) -> ast::ExprBlock {
    let syntax = &module_syntax.items(db).elements(db)[index];
    let function_syntax = match syntax {
        ast::Item::Function(function_syntax) => function_syntax,
        _ => panic!("Not a free function."),
    };
    function_syntax.body(db)
}
