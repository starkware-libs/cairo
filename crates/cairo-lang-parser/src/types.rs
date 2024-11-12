use std::fmt::Display;

use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_syntax::node::{db::SyntaxGroup, SyntaxNode};
use num_traits::ToPrimitive;

pub trait Tokenable {
    fn to_tokens(&self, db: &dyn SyntaxGroup) -> Vec<Token>;
    fn len(&self, db: &dyn SyntaxGroup) -> u32;
}

/// Used to run Cairo language parser over a custom source of tokens not produced by the lexer.
pub trait TokenStream: Display + Tokenable {
    /// Returns the starting [`TextOffset`] of the token stream, if there is at least a single
    /// token.
    ///
    /// This property is used as offset of a root [cairo_lang_syntax::node::SyntaxNode] produced
    /// when parsing this stream.
    fn get_start_offset(&self) -> Option<TextOffset>;

    fn new(tokens: Vec<Token>) -> Self;
}

#[derive(Clone, Debug)]
pub struct Token {
    pub content: String,
    pub span: TextSpan,
}

impl Token {
    pub fn new(content: String, span: TextSpan) -> Self {
        Self { content, span }
    }

    pub fn new_with_offset(content: &String, offset: u32) -> Self {
        let offset_width = TextWidth::new(offset);
        Self {
            content: content.clone(),
            span: TextSpan {
                start: TextOffset::default().add_width(offset_width),
                end: TextOffset::default()
                    .add_width(offset_width)
                    .add_width(TextWidth::from_str(content)),
            },
        }
    }

    pub fn token_from_syntax_node(db: &dyn SyntaxGroup, node: SyntaxNode) -> Token {
        Self::new(node.get_text(db), node.span(db))
    }
}

#[derive(Debug)]
pub struct MockTokenStream {
    pub tokens: Vec<Token>,
    pub content_string: String,
}

impl MockTokenStream {
    pub fn from_syntax_node(db: &dyn SyntaxGroup, node: SyntaxNode) -> Self {
        let leaves = node.tokens(db);
        let tokens = leaves.map(|node| Token::token_from_syntax_node(db, node.clone())).collect();
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
    #[doc(hidden)]
    fn new(tokens: Vec<Token>) -> Self {
        let content_string = tokens.iter().map(|token| token.content.clone()).collect::<String>();
        Self { tokens, content_string }
    }

    fn get_start_offset(&self) -> Option<TextOffset> {
        self.tokens.first().map(|token| token.span.start)
    }
}

impl Tokenable for MockTokenStream {
    fn to_tokens(&self, _db: &dyn SyntaxGroup) -> Vec<Token> {
        self.tokens.clone()
    }

    fn len(&self, _db: &dyn SyntaxGroup) -> u32 {
        self.tokens.iter().map(|token| token.content.len()).sum::<usize>() as u32
    }
}

impl Tokenable for SyntaxNode {
    fn to_tokens(&self, db: &dyn SyntaxGroup) -> Vec<Token> {
        vec![Token::new(self.get_text(db), self.span(db))]
    }

    fn len(&self, db: &dyn SyntaxGroup) -> u32 {
        self.get_text(db).len() as u32
    }
}
