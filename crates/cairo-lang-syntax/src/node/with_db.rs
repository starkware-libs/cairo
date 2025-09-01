use cairo_lang_filesystem::span::TextWidth;
use cairo_lang_primitive_token::{PrimitiveSpan, PrimitiveToken, ToPrimitiveTokenStream};
use salsa::Database;

use super::SyntaxNode;

pub struct SyntaxNodeWithDb<'a> {
    node: &'a SyntaxNode<'a>,
    db: &'a dyn Database,
}

impl<'a> SyntaxNodeWithDb<'a> {
    pub fn new(node: &'a SyntaxNode<'a>, db: &'a dyn Database) -> Self {
        Self { node, db }
    }
}

impl<'a> ToPrimitiveTokenStream for SyntaxNodeWithDb<'a> {
    type Iter = SyntaxNodeWithDbIterator<'a>;

    fn to_primitive_token_stream(&self) -> Self::Iter {
        // The lifetime of the iterator should extend 'a because it derives from both node and db
        SyntaxNodeWithDbIterator::new(self.db, self.node)
    }
}

pub struct SyntaxNodeWithDbIterator<'a> {
    /// Stack used for driving iterative depth-first traversal of the syntax tree.
    iter_stack: Vec<&'a SyntaxNode<'a>>,
    /// Each step of the traversal may yield up to three tokens, so we collect them in this buffer.
    /// **INVARIANT**: The collection to the buffer only happens when the buffer was previously
    /// empty.
    buffer: Vec<PrimitiveToken>,
    db: &'a dyn Database,
}

impl<'a> SyntaxNodeWithDbIterator<'a> {
    pub fn new(db: &'a dyn Database, node: &'a SyntaxNode<'a>) -> Self {
        Self { db, buffer: Vec::with_capacity(3), iter_stack: vec![node] }
    }
}

impl<'a> Iterator for SyntaxNodeWithDbIterator<'a> {
    type Item = PrimitiveToken;

    fn next(&mut self) -> Option<Self::Item> {
        // If the buffer is empty, perform a single step of the depth-first traversal.
        // This will fill the buffer with up to three tokens from the current node.
        // If the stack is empty, it means we have traversed the entire tree and the function will
        // return `None`, ending the iteration.
        loop {
            // INVARIANT: Empty the buffer before traversing further.
            if let Some(item) = self.buffer.pop() {
                // Return the next token from the buffer.
                return Some(item);
            }
            let node = self.iter_stack.pop()?;
            // This represents a single step of the depth-first traversal of a syntax tree.
            // If the node is a terminal, it creates and saves token representation of it.
            // Otherwise, it pushes all children of the node onto the iteration stack.
            if node.green_node(self.db).kind.is_terminal() {
                token_from_syntax_node(node, self.db, &mut self.buffer);
            } else {
                self.iter_stack.extend(node.get_children(self.db).iter().rev());
            }
        }
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
///
/// Each of these `PrimitiveToken` structs is pushed into the `result` queue.
///
/// **INVARIANT**: `result` **MUST** be empty when passed to this function.
fn token_from_syntax_node(
    node: &SyntaxNode<'_>,
    db: &dyn Database,
    result: &mut Vec<PrimitiveToken>,
) {
    let span_without_trivia = node.span_without_trivia(db);
    let span_with_trivia = node.span(db);
    let text = node.get_text(db);
    let prefix_len = span_without_trivia.start - span_with_trivia.start;
    let (prefix, rest) = text.split_at(prefix_len.as_u32() as usize);
    let suffix_len = span_with_trivia.end - span_without_trivia.end;
    let (content, suffix) = rest.split_at(rest.len() - suffix_len.as_u32() as usize);
    if suffix_len > TextWidth::ZERO {
        result.push(PrimitiveToken {
            content: suffix.to_string(),
            span: Some(PrimitiveSpan {
                start: span_without_trivia.end.as_u32() as usize,
                end: span_with_trivia.end.as_u32() as usize,
            }),
        });
    }
    if !content.is_empty() {
        result.push(PrimitiveToken {
            content: content.to_string(),
            span: Some(PrimitiveSpan {
                start: span_without_trivia.start.as_u32() as usize,
                end: span_without_trivia.end.as_u32() as usize,
            }),
        });
    }
    if prefix_len > TextWidth::ZERO {
        result.push(PrimitiveToken {
            content: prefix.to_string(),
            span: Some(PrimitiveSpan {
                start: span_with_trivia.start.as_u32() as usize,
                end: span_without_trivia.start.as_u32() as usize,
            }),
        });
    }
}
