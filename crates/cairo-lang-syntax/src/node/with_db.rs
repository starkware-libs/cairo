use cairo_lang_filesystem::span::TextWidth;
use cairo_lang_primitive_token::{PrimitiveSpan, PrimitiveToken, ToPrimitiveTokenStream};

use super::SyntaxNode;
use super::db::SyntaxGroup;

pub struct SyntaxNodeWithDb<'a, Db: SyntaxGroup> {
    node: &'a SyntaxNode,
    db: &'a Db,
}

impl<'a, Db: SyntaxGroup> SyntaxNodeWithDb<'a, Db> {
    pub fn new(node: &'a SyntaxNode, db: &'a Db) -> Self {
        Self { node, db }
    }
}

impl<'a, Db: SyntaxGroup> ToPrimitiveTokenStream for SyntaxNodeWithDb<'a, Db> {
    type Iter = SyntaxNodeWithDbIterator<'a, Db>;

    fn to_primitive_token_stream(&self) -> Self::Iter {
        // The lifetime of the iterator should extend 'a because it derives from both node and db
        SyntaxNodeWithDbIterator::new(
            Box::new(
                self.node.tokens(self.db).flat_map(|node| token_from_syntax_node(node, self.db)),
            ),
            self.db,
        )
    }
}

pub struct SyntaxNodeWithDbIterator<'a, Db: SyntaxGroup> {
    inner: Box<dyn Iterator<Item = PrimitiveToken> + 'a>,
    _db: &'a Db,
}

impl<'a, Db: SyntaxGroup> SyntaxNodeWithDbIterator<'a, Db> {
    pub fn new(inner: Box<dyn Iterator<Item = PrimitiveToken> + 'a>, db: &'a Db) -> Self {
        Self { inner, _db: db }
    }
}

impl<Db: SyntaxGroup> Iterator for SyntaxNodeWithDbIterator<'_, Db> {
    type Item = PrimitiveToken;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

/// Create a `PrimitiveToken` representation from a `SyntaxNode`.
///
/// The `PrimitiveToken` keeps two information - some text content and a span associated with it.
/// A `SyntaxNode` consists of some token, trivia associated with this token, and a span describing
/// the token either with or without trivia.
/// We split the content of the `SyntaxNode` into three parts: the prefix trivia, the main content
/// of the node, and the suffix trivia. Each part is represented as a separate `PrimitiveToken`
/// with its corresponding span.
fn token_from_syntax_node(node: SyntaxNode, db: &dyn SyntaxGroup) -> Vec<PrimitiveToken> {
    let span_without_trivia = node.span_without_trivia(db);
    let span_with_trivia = node.span(db);
    let text = node.get_text(db);
    let mut result = Vec::new();
    let prefix_len = span_without_trivia.start - span_with_trivia.start;
    let (prefix, rest) = text.split_at(prefix_len.as_u32() as usize);
    if prefix_len > TextWidth::ZERO {
        result.push(PrimitiveToken {
            content: prefix.to_string(),
            span: Some(PrimitiveSpan {
                start: span_with_trivia.start.as_u32() as usize,
                end: span_without_trivia.start.as_u32() as usize,
            }),
        });
    }
    let suffix_len = span_with_trivia.end - span_without_trivia.end;
    let (content, suffix) = rest.split_at(rest.len() - suffix_len.as_u32() as usize);
    if !content.is_empty() {
        result.push(PrimitiveToken {
            content: content.to_string(),
            span: Some(PrimitiveSpan {
                start: span_without_trivia.start.as_u32() as usize,
                end: span_without_trivia.end.as_u32() as usize,
            }),
        });
    }
    if suffix_len > TextWidth::ZERO {
        result.push(PrimitiveToken {
            content: suffix.to_string(),
            span: Some(PrimitiveSpan {
                start: span_without_trivia.end.as_u32() as usize,
                end: span_with_trivia.end.as_u32() as usize,
            }),
        });
    }
    result
}
