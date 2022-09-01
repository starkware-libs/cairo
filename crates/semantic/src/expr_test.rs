use std::sync::Arc;

use assert_matches::assert_matches;
use defs::db::{AsDefsGroup, DefsDatabase, DefsGroup};
use filesystem::db::{FilesDatabase, FilesGroup, ProjectConfig};
use filesystem::ids::{CrateLongId, FileLongId, ModuleId, VirtualFile};
use indoc::indoc;
use parser::db::{ParserDatabase, ParserGroup};
use syntax::node::ast;
use syntax::node::db::{AsSyntaxGroup, SyntaxDatabase, SyntaxGroup};

use super::compute_expr_semantic;
use crate::corelib::unit_ty;
use crate::db::{SemanticDatabase, SemanticGroup};
use crate::expr::ComputationContext;
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
    let mut db_val = DatabaseImpl::default();
    let module_id = setup_test_module(&mut db_val, "func foo() { 7 }");
    let db = &db_val;
    let module_syntax = db.file_syntax(db.module_file(module_id).unwrap()).expect("").unwrap();
    // TODO(spapini): When a tail expression is supported, take the syntax from the tail instead
    // of the statements.
    let syntax = match &extract_function_body(db, module_syntax, 0).statements(db).elements(db)[0] {
        ast::Statement::Expr(syntax) => syntax.expr(db),
        _ => panic!("Expected an expression statemnet"),
    };

    // Compute semantics of expr.
    let mut ctx = ComputationContext { db, module_id };
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
fn test_expr_block() {
    let mut db_val = DatabaseImpl::default();
    let module_id = setup_test_module(
        &mut db_val,
        indoc! {"
            func foo() {
                6;
                8;
            }
        "},
    );
    let db = &db_val;
    let module_syntax = db.file_syntax(db.module_file(module_id).unwrap()).expect("").unwrap();
    let syntax = ast::Expr::Block(extract_function_body(db, module_syntax, 0));

    // Compute semantics of expr.
    let mut ctx = ComputationContext { db, module_id };
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
    let module_id = setup_test_module(
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
    let module_syntax = db.file_syntax(db.module_file(module_id).unwrap()).expect("").unwrap();
    // TODO(spapini): When a tail expression in a block is supported, take the syntax from the tail
    // instead of from the statments.
    let syntax = match &extract_function_body(db, module_syntax, 0).statements(db).elements(db)[0] {
        ast::Statement::Expr(syntax) => syntax.expr(db),
        _ => panic!("Expected an expression statement"),
    };

    // Compute semantics of expr.
    let mut ctx = ComputationContext { db, module_id };
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

fn setup_test_module(db: &mut DatabaseImpl, content: &str) -> ModuleId {
    let crate_id = db.intern_crate(CrateLongId("test_crate".into()));
    let file_id = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "test.cairo".into(),
        content: Arc::new(content.to_string()),
    }));
    db.set_project_config(ProjectConfig {
        crate_roots: [(crate_id, file_id)].into_iter().collect(),
    });
    ModuleId::CrateRoot(crate_id)
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
