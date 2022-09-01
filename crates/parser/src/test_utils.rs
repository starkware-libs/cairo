use std::sync::Arc;

use filesystem::ids::{FileLongId, VirtualFile};
use syntax::node::{ast, SyntaxNode, TypedSyntaxNode};

use crate::db::ParserGroup;
use crate::parser::Parser;

pub fn prepare_test_expr(db: &dyn ParserGroup, content: &str) -> ast::Expr {
    let mut parser = prepare_test_parser(content, db);
    let green = parser.parse_expr();
    let syntax_node = SyntaxNode::new_root(db.as_syntax_group(), green);
    ast::Expr::from_syntax_node(db.as_syntax_group(), syntax_node)
}

pub fn prepare_test_syntax_file(db: &dyn ParserGroup, content: &str) -> ast::SyntaxFile {
    let parser = prepare_test_parser(content, db);
    parser.parse_syntax_file().expect("Unexpected parsing error")
}

fn prepare_test_parser<'db: 'content, 'content>(
    content: &'content str,
    db: &'db dyn ParserGroup,
) -> Parser<'content> {
    let content_clone = Arc::new(content.to_string());
    let file = db.intern_file(FileLongId::Virtual(VirtualFile {
        parent: None,
        name: "test.cairo".into(),
        content: content_clone,
    }));
    Parser::from_text(db.as_syntax_group(), file, content)
}
