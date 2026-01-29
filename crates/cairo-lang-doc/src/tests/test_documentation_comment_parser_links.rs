use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::ModuleId;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_syntax::node::SyntaxNode;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use salsa::Database;

use super::test_utils::{TestDatabase, setup_test_module, test_crate_id};
use crate::db::DocGroup;

cairo_lang_test_utils::test_file_test!(
    documentation_comment_parser_links,
    "src/tests/test-data",
    {
        documentation_comment_parser_links: "documentation_comment_parser_links.txt",
    },
    documentation_comment_parser_links_runner
);

fn collect_doc_comments<'db>(
    db: &'db dyn Database,
    node: SyntaxNode<'db>,
    comments: &mut Vec<SyntaxNode<'db>>,
) {
    if node.kind(db) == SyntaxKind::TokenSingleLineDocComment {
        comments.push(node);
    }
    for &child in node.get_children(db) {
        collect_doc_comments(db, child, comments);
    }
}

fn documentation_comment_parser_links_runner(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let mut db = TestDatabase::new().unwrap();
    setup_test_module(&mut db, inputs["cairo_code"].as_str());

    let crate_id = test_crate_id(&db);
    let file_id = db.module_main_file(ModuleId::CrateRoot(crate_id)).unwrap();
    let syntax_file = db.file_syntax(file_id).unwrap();
    let mut doc_comments = Vec::new();
    collect_doc_comments(&db, syntax_file, &mut doc_comments);

    let mut output = OrderedHashMap::default();
    for (index, doc_comment) in doc_comments.iter().enumerate() {
        let counter = index + 1;
        let content = doc_comment.get_text(&db);
        let embedded_links = db.get_embedded_markdown_links(*doc_comment);
        output.insert(format!("Doc comment #{counter}"), content.to_string());
        output.insert(
            format!("Doc comment #{counter} links"),
            embedded_links.iter().map(|link| format!("{link:?}")).collect::<Vec<_>>().join("\n"),
        );
    }

    TestRunnerResult::success(output)
}
