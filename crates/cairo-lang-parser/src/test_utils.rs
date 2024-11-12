use std::fmt::Display;

use cairo_lang_filesystem::ids::{FileId, FileKind, FileLongId, VirtualFile};
use cairo_lang_filesystem::span::{TextOffset, TextSpan};
use cairo_lang_syntax::node::SyntaxNode;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_test_utils::parse_test_file::TestRunnerResult;
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use smol_str::SmolStr;

use crate::types::TokenStream;
use crate::utils::{SimpleParserDatabase, get_syntax_root_and_diagnostics};

pub fn get_diagnostics(
    inputs: &OrderedHashMap<String, String>,
    _args: &OrderedHashMap<String, String>,
) -> TestRunnerResult {
    let db = &SimpleParserDatabase::default();
    let code = &inputs["cairo_code"];

    let file_id = create_virtual_file(db, "dummy_file.cairo", code);
    let (_, diagnostics) = get_syntax_root_and_diagnostics(db, file_id, code);
    TestRunnerResult::success(OrderedHashMap::from([(
        "expected_diagnostics".into(),
        diagnostics.format(db),
    )]))
}

// TODO(yuval): stop virtual files for tests anymore. See semantic tests.
/// Creates a virtual file with the given content and returns its ID.
pub fn create_virtual_file(
    db: &SimpleParserDatabase,
    file_name: impl Into<SmolStr>,
    content: &str,
) -> FileId {
    FileLongId::Virtual(VirtualFile {
        parent: None,
        name: file_name.into(),
        content: content.into(),
        code_mappings: [].into(),
        kind: FileKind::Module,
    })
    .intern(db)
}

/// Mocked struct which implements [crate::types::TokenStream]
/// It's main purpose is being used for testing the [crate::parser::Parser::parse_token_stream]
#[derive(Debug, Clone)]
pub struct MockTokenStream {
    /// Field that holds all the tokens that are part of the stream
    pub tokens: Vec<MockToken>,
    /// It's just a field to store the content of all of the tokens, so we can implement a
    /// [crate::types::TokenStream::as_str]
    pub content_string: String,
}

/// Represent a token inside the [MockTokenStream]
#[derive(Debug, Default, Clone)]
pub struct MockToken {
    /// Just a text of a given [MockToken]
    pub content: String,
    /// It's offsets are related to the other tokens present in the same [MockTokenStream].
    pub span: TextSpan,
}

impl MockToken {
    pub fn new(content: String, span: TextSpan) -> Self {
        Self { content, span }
    }

    /// Create a token based on [SyntaxNode]
    pub fn from_syntax_node(db: &dyn SyntaxGroup, node: SyntaxNode) -> MockToken {
        MockToken::new(node.get_text(db), node.span(db))
    }
}

impl MockTokenStream {
    #[doc(hidden)]
    pub fn new(tokens: Vec<MockToken>) -> Self {
        let content_string = tokens.iter().map(|token| token.content.clone()).collect::<String>();
        Self { tokens, content_string }
    }

    /// Create whole [MockTokenStream] based upon the [SyntaxNode].
    pub fn from_syntax_node(db: &dyn SyntaxGroup, node: SyntaxNode) -> Self {
        let leaves = node.tokens(db);
        let tokens = leaves.map(|node| MockToken::from_syntax_node(db, node.clone())).collect();
        Self::new(tokens)
    }
}

impl Display for MockTokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for token in &self.tokens {
            write!(f, "{}", token.content.clone())?;
        }
        Ok(())
    }
}

impl TokenStream for MockTokenStream {
    fn get_start_offset(&self) -> Option<TextOffset> {
        self.tokens.first().map(|token| token.span.start)
    }

    fn as_str(&self) -> &str {
        &self.content_string
    }
}
