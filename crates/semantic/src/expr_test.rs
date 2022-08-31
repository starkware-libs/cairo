use std::sync::Arc;

use assert_matches::assert_matches;
use defs::db::{AsDefsGroup, DefsDatabase, DefsGroup};
use filesystem::db::{FilesDatabase, FilesGroup};
use filesystem::ids::{FileLongId, VirtualFile};
use indoc::indoc;
use parser::db::ParserDatabase;
use parser::parser::Parser;
use syntax::node::db::{AsSyntaxGroup, SyntaxDatabase, SyntaxGroup};
use syntax::node::{ast, SyntaxNode, TypedSyntaxNode};

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

fn setup(content: &str) -> (DatabaseImpl, SyntaxNode) {
    let db_val = DatabaseImpl::default();
    let db = &db_val;
    let content = Arc::new(content.to_string());
    let file = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "test.cairo".into(),
        content: content.clone(),
    }));
    let mut parser = Parser::from_text(db, file, &content);
    let green = parser.parse_expr();
    let syntax_node = SyntaxNode::new_root(db, green);
    (db_val, syntax_node)
}

#[test]
fn test_expr_literal() {
    let (db_val, syntax_node) = setup("7");
    let db = &db_val;

    // Compute semantics of expr.
    let syntax = ast::Expr::from_syntax_node(db, syntax_node);
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
    let (db_val, syntax_node) = setup(indoc! {"
        {
            6;
            8;
        }
    "});
    let db = &db_val;

    // Compute semantics of expr.
    let syntax = ast::Expr::from_syntax_node(db, syntax_node);
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
