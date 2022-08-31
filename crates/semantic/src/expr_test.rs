use std::sync::Arc;

use defs::db::{AsDefsGroup, DefsDatabase, DefsGroup};
use filesystem::db::{FilesDatabase, FilesGroup};
use filesystem::ids::{FileLongId, VirtualFile};
use parser::db::ParserDatabase;
use parser::parser::Parser;
use syntax::node::db::{AsSyntaxGroup, SyntaxDatabase, SyntaxGroup};
use syntax::node::{ast, SyntaxNode, TypedSyntaxNode};

use super::compute_expr_semantic;
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
fn test_expr() {
    let db_val = DatabaseImpl::default();
    let db = &db_val;
    let content = Arc::new("7".to_string());
    let file = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "test.cairo".into(),
        content: content.clone(),
    }));
    let mut parser = Parser::from_text(db, file, &content);
    let green = parser.try_parse_atom().unwrap();
    let syntax_node = SyntaxNode::new_root(db, green);
    let syntax = ast::Expr::from_syntax_node(db, syntax_node);
    let expr_id = compute_expr_semantic(db, syntax);
    let expr = db.lookup_intern_expr(expr_id);
    let semantic::ExprLiteral { value, ty } = match expr {
        crate::Expr::ExprLiteral(expr) => expr,
        _ => panic!("Expected a literal."),
    };
    assert_eq!(value, 7);
    assert_eq!(ty, db.core_felt_ty());
}
