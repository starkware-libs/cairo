use cairo_lang_primitive_token::{PrimitiveSpan, PrimitiveToken, ToPrimitiveTokenStream};
use cairo_lang_syntax::node::ids::GreenId;
use cairo_lang_syntax::node::{SyntaxNode, both_trivia_width};
use cairo_lang_utils::LookupIntern;

use crate::db::{ParserGroup, SyntaxNodeExt};

pub struct SyntaxNodeWithDb<'a, Db: ParserGroup> {
    node: &'a SyntaxNode,
    db: &'a Db,
}

impl<'a, Db: ParserGroup> SyntaxNodeWithDb<'a, Db> {
    pub fn new(node: &'a SyntaxNode, db: &'a Db) -> Self {
        Self { node, db }
    }
}

impl<'a, Db: ParserGroup> ToPrimitiveTokenStream for SyntaxNodeWithDb<'a, Db> {
    type Iter = SyntaxNodeWithDbIterator<'a, Db>;

    fn to_primitive_token_stream(&self) -> Self::Iter {
        let file_content = self
            .db
            .file_content(self.node.stable_ptr(self.db).file_id(self.db))
            .expect("Failed to read file content");
        let mut offset = self.node.offset(self.db).as_u32() as usize;
        let db = self.db;
        // The lifetime of the iterator should extend 'a because it derives from both node and db
        SyntaxNodeWithDbIterator::new(
            Box::new(self.node.tokens(db).into_iter().flat_map(move |green| {
                token_from_syntax_node(&file_content, &mut offset, green, db)
            })),
            self.db,
        )
    }
}

pub struct SyntaxNodeWithDbIterator<'a, Db: ParserGroup> {
    inner: Box<dyn Iterator<Item = PrimitiveToken> + 'a>,
    _db: &'a Db,
}

impl<'a, Db: ParserGroup> SyntaxNodeWithDbIterator<'a, Db> {
    pub fn new(inner: Box<dyn Iterator<Item = PrimitiveToken> + 'a>, db: &'a Db) -> Self {
        Self { inner, _db: db }
    }
}

impl<Db: ParserGroup> Iterator for SyntaxNodeWithDbIterator<'_, Db> {
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
fn token_from_syntax_node(
    file_content: &str,
    offset_mut: &mut usize,
    green: GreenId,
    db: &dyn ParserGroup,
) -> Vec<PrimitiveToken> {
    let mut result = vec![];
    let green_node = green.lookup_intern(db);
    debug_assert!(green_node.kind.is_terminal());
    let (leading_trivia, trailing_trivia) = both_trivia_width(db, &green_node);
    let leading_trivia = leading_trivia.as_u32() as usize;
    let trailing_trivia = trailing_trivia.as_u32() as usize;
    let offset = *offset_mut;

    let token_width = green_node.children()[1].lookup_intern(db).width().as_u32() as usize;
    let after_leading_trivia = offset + leading_trivia;
    let after_token = after_leading_trivia + token_width;
    let after_trailing_trivia = after_token + trailing_trivia;

    if leading_trivia != 0 {
        result.push(PrimitiveToken {
            content: file_content[offset..after_leading_trivia].to_string(),
            span: Some(PrimitiveSpan { start: offset, end: after_leading_trivia }),
        });
    }

    if token_width != 0 {
        result.push(PrimitiveToken {
            content: file_content[after_leading_trivia..after_token].to_string(),
            span: Some(PrimitiveSpan { start: after_leading_trivia, end: after_token }),
        });
    }

    if trailing_trivia != 0 {
        result.push(PrimitiveToken {
            content: file_content[after_token..after_trailing_trivia].to_string(),
            span: Some(PrimitiveSpan { start: after_token, end: after_trailing_trivia }),
        });
    }

    *offset_mut += green_node.width().as_u32() as usize;

    result
}
