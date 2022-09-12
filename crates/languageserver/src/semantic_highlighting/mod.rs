use filesystem::span::TextOffset;
use syntax::node::ast::{self};
use syntax::node::db::SyntaxGroup;
use syntax::node::kind::SyntaxKind;
use syntax::node::{SyntaxNode, TypedSyntaxNode};
use tower_lsp::lsp_types::*;
use utils::unordered_hash_map::UnorderedHashMap;

use self::encoder::{EncodedToken, TokenEncoder};
use self::token_kind::SemanticTokenKind;

mod encoder;
pub mod token_kind;

#[derive(Default)]
pub struct SemanticTokensTraverser {
    encoder: TokenEncoder,
    /// A map from an offset in the file to semantic token kind.
    /// This map is used to override future tokens based on the context.
    /// For example: when we see the "func" keyword, the name token is added
    /// to the map, so that instead of marking it as an identifier, we will mark it
    /// as a function name.
    offset_to_kind_lookahead: UnorderedHashMap<u32, SemanticTokenKind>,
}
impl SemanticTokensTraverser {
    pub fn find_semantic_tokens(
        &mut self,
        db: &dyn SyntaxGroup,
        data: &mut Vec<SemanticToken>,
        node: SyntaxNode,
    ) {
        let green_node = node.green_node(db);
        match green_node.details {
            syntax::node::green::GreenNodeDetails::Token(text) => {
                if green_node.kind == SyntaxKind::TokenNewline {
                    self.encoder.next_line();
                    return;
                }

                let width = text.len() as u32;
                let maybe_semantic_kind = self
                    .offset_to_kind_lookahead
                    .remove(&(node.offset().0 as u32))
                    .or_else(|| SemanticTokenKind::from_syntax_kind(green_node.kind));
                if let Some(semantic_kind) = maybe_semantic_kind {
                    let EncodedToken { delta_line, delta_start } = self.encoder.encode(width);
                    data.push(SemanticToken {
                        delta_line,
                        delta_start,
                        length: width,
                        token_type: semantic_kind.as_u32(),
                        token_modifiers_bitset: 0,
                    });
                } else {
                    self.encoder.skip(width);
                }
            }
            syntax::node::green::GreenNodeDetails::Node { .. } => {
                let children = node.children(db);
                match green_node.kind {
                    SyntaxKind::Param => {
                        self.mark_future_token(
                            ast::Param::from_syntax_node(db, node)
                                .name(db)
                                .as_syntax_node()
                                .offset(),
                            SemanticTokenKind::Parameter,
                        );
                    }
                    SyntaxKind::ItemFreeFunction => {
                        self.mark_future_token(
                            ast::ItemFreeFunction::from_syntax_node(db, node)
                                .name(db)
                                .as_syntax_node()
                                .offset(),
                            SemanticTokenKind::Function,
                        );
                    }
                    SyntaxKind::ItemStruct => self.mark_future_token(
                        ast::ItemStruct::from_syntax_node(db, node)
                            .name(db)
                            .as_syntax_node()
                            .offset(),
                        SemanticTokenKind::Struct,
                    ),
                    SyntaxKind::ItemEnum => self.mark_future_token(
                        ast::ItemEnum::from_syntax_node(db, node)
                            .name(db)
                            .as_syntax_node()
                            .offset(),
                        SemanticTokenKind::Enum,
                    ),
                    _ => {}
                }
                for child in children {
                    self.find_semantic_tokens(db, data, child);
                }
            }
        }
    }

    fn mark_future_token(&mut self, offset: TextOffset, semantic_kind: SemanticTokenKind) {
        self.offset_to_kind_lookahead.insert(offset.0 as u32, semantic_kind);
    }
}
