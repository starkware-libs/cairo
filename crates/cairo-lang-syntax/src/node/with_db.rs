use cairo_lang_stable_token::{StableSpan, StableToken, ToStableTokenStream};

use super::SyntaxNode;
use super::db::SyntaxGroup;

pub struct SyntaxNodeWithDb<'a, Db: SyntaxGroup> {
    node: SyntaxNode,
    db: &'a Db,
}

impl<'a, Db: SyntaxGroup> SyntaxNodeWithDb<'a, Db> {
    pub fn new(node: SyntaxNode, db: &'a Db) -> Self {
        Self { node, db }
    }
}

impl<'a, Db: SyntaxGroup> ToStableTokenStream for SyntaxNodeWithDb<'a, Db> {
    type Iter = Box<dyn Iterator<Item = StableToken> + 'a>;
    fn to_stable_token_stream(&self) -> Self::Iter {
        let tokens: Vec<StableToken> = self
            .node
            .tokens(self.db)
            .map(|token| {
                let span = token.span(self.db).to_str_range();
                StableToken::new(
                    token.get_text(self.db),
                    Some(StableSpan { start: span.start, end: span.end }),
                )
            })
            .collect();

        Box::new(tokens.into_iter())
    }
}
