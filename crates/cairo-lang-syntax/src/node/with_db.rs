use std::collections::VecDeque;

use cairo_lang_filesystem::span::TextWidth;
use cairo_lang_primitive_token::{PrimitiveSpan, PrimitiveToken, ToPrimitiveTokenStream};

use super::SyntaxNode;
use super::db::SyntaxGroup;

pub struct SyntaxNodeWithDb<'a, Db: SyntaxGroup> {
    node: &'a SyntaxNode<'a>,
    db: &'a Db,
}

impl<'a, Db: SyntaxGroup> SyntaxNodeWithDb<'a, Db> {
    pub fn new(node: &'a SyntaxNode<'a>, db: &'a Db) -> Self {
        Self { node, db }
    }
}

impl<'a, Db: SyntaxGroup> ToPrimitiveTokenStream for SyntaxNodeWithDb<'a, Db> {
    type Iter = SyntaxNodeWithDbIterator<'a, Db>;

    fn to_primitive_token_stream(&self) -> Self::Iter {
        // The lifetime of the iterator should extend 'a because it derives from both node and db
        SyntaxNodeWithDbIterator::new(self.db, self.node)
    }
}

pub struct SyntaxNodeWithDbIterator<'a, Db: SyntaxGroup> {
    /// Stack used for driving iterative depth-first traversal of the syntax tree.
    iter_stack: Vec<&'a SyntaxNode<'a>>,
    /// Each step of the traversal may yield up to three tokens, so we collect them in this buffer.
    buffer: VecDeque<PrimitiveToken>,
    db: &'a Db,
}

impl<'a, Db: SyntaxGroup> SyntaxNodeWithDbIterator<'a, Db> {
    pub fn new(db: &'a Db, node: &'a SyntaxNode<'a>) -> Self {
        Self { db, buffer: Default::default(), iter_stack: vec![node] }
    }
}

impl<Db: SyntaxGroup> Iterator for SyntaxNodeWithDbIterator<'_, Db> {
    type Item = PrimitiveToken;

    fn next(&mut self) -> Option<Self::Item> {
        // If the buffer is empty, perform a single step of the depth-first traversal.
        // This will fill the buffer with up to three tokens from the current node.
        // If the stack is empty, it means we have traversed the entire tree and the function will
        // return `None`, ending the iteration.
        loop {
            if let Some(item) = self.buffer.pop_front() {
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
fn token_from_syntax_node(
    node: &SyntaxNode<'_>,
    db: &dyn SyntaxGroup,
    result: &mut VecDeque<PrimitiveToken>,
) {
    let span_without_trivia = node.span_without_trivia(db);
    let span_with_trivia = node.span(db);
    let text = node.get_text(db);
    let prefix_len = span_without_trivia.start - span_with_trivia.start;
    let (prefix, rest) = text.split_at(prefix_len.as_u32() as usize);
    if prefix_len > TextWidth::ZERO {
        result.push_back(PrimitiveToken {
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
        result.push_back(PrimitiveToken {
            content: content.to_string(),
            span: Some(PrimitiveSpan {
                start: span_without_trivia.start.as_u32() as usize,
                end: span_without_trivia.end.as_u32() as usize,
            }),
        });
    }
    if suffix_len > TextWidth::ZERO {
        result.push_back(PrimitiveToken {
            content: suffix.to_string(),
            span: Some(PrimitiveSpan {
                start: span_without_trivia.end.as_u32() as usize,
                end: span_with_trivia.end.as_u32() as usize,
            }),
        });
    }
}
