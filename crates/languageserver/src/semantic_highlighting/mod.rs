use std::collections::HashMap;

use filesystem::span::TextOffset;
use syntax::node::ast::{self};
use syntax::node::db::SyntaxGroup;
use syntax::node::kind::SyntaxKind;
use syntax::node::{SyntaxNode, TypedSyntaxNode};
use syntax::token::TokenKind;
use tower_lsp::lsp_types::*;

use self::encoder::TokenEncoder;
use self::token_kind::SemanticTokenKind;

mod encoder;
pub mod token_kind;

#[derive(Default)]
pub struct SemanticTokensTraverser {
    encoder: TokenEncoder,
    table: HashMap<u32, SemanticTokenKind>,
}
impl SemanticTokensTraverser {
    pub fn find_semantic_tokens(
        &mut self,
        db: &dyn SyntaxGroup,
        data: &mut Vec<SemanticToken>,
        node: SyntaxNode,
    ) {
        let children = node.children(db);
        match &node.details(db) {
            syntax::node::SyntaxNodeDetails::Syntax(kind) => {
                match kind {
                    SyntaxKind::Param => {
                        self.at_offset(
                            ast::Param::from_syntax_node(db, node)
                                .name(db)
                                .as_syntax_node()
                                .offset(),
                            SemanticTokenKind::Parameter,
                        );
                    }
                    SyntaxKind::ItemFreeFunction => {
                        self.at_offset(
                            ast::ItemFreeFunction::from_syntax_node(db, node)
                                .name(db)
                                .as_syntax_node()
                                .offset(),
                            SemanticTokenKind::Function,
                        );
                    }
                    SyntaxKind::ItemStruct => self.at_offset(
                        ast::ItemStruct::from_syntax_node(db, node)
                            .name(db)
                            .as_syntax_node()
                            .offset(),
                        SemanticTokenKind::Struct,
                    ),
                    SyntaxKind::ItemEnum => self.at_offset(
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
            syntax::node::SyntaxNodeDetails::Token(token) => {
                if token.kind == TokenKind::Newline {
                    self.encoder.newline();
                    return;
                }
                let width = token.width();
                if let Some(semantic_kind) = self
                    .table
                    .remove(&(node.offset().0 as u32))
                    .or_else(|| SemanticTokenKind::from_token_kind(token.kind))
                {
                    let (delta_line, delta_start) = self.encoder.encode(width);
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
        }
    }

    fn at_offset(&mut self, offset: TextOffset, semantic_kind: SemanticTokenKind) {
        self.table.insert(offset.0 as u32, semantic_kind);
    }
}
