use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::TextOffset;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::ast::{self};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::Upcast;
use tower_lsp::lsp_types::*;
use tracing::error;

use self::encoder::{EncodedToken, TokenEncoder};
pub use self::token_kind::SemanticTokenKind;
use crate::lang::lsp::LsProtoGroup;

mod encoder;
mod token_kind;

/// Resolve the semantic tokens of a given file.
#[tracing::instrument(
    level = "debug",
    skip_all,
    fields(uri = %params.text_document.uri)
)]
pub fn semantic_highlight_full(
    params: SemanticTokensParams,
    db: &RootDatabase,
) -> Option<SemanticTokensResult> {
    let file_uri = params.text_document.uri;
    let file = db.file_for_url(&file_uri);
    let Ok(node) = db.file_syntax(file) else {
        error!("semantic analysis failed: file '{file_uri}' does not exist");
        return None;
    };

    let mut data: Vec<SemanticToken> = Vec::new();
    SemanticTokensTraverser::default().find_semantic_tokens(db.upcast(), file, &mut data, node);
    Some(SemanticTokensResult::Tokens(SemanticTokens { result_id: None, data }))
}

#[derive(Default)]
struct SemanticTokensTraverser {
    encoder: TokenEncoder,
    /// A map from an offset in the file to semantic token kind.
    /// This map is used to override future tokens based on the context.
    /// For example: when we see the "fn" keyword, the name token is added
    /// to the map, so that instead of marking it as an identifier, we will mark it
    /// as a function name.
    offset_to_kind_lookahead: UnorderedHashMap<TextOffset, SemanticTokenKind>,
}
impl SemanticTokensTraverser {
    pub fn find_semantic_tokens(
        &mut self,
        db: &dyn SemanticGroup,
        file_id: FileId,
        data: &mut Vec<SemanticToken>,
        node: SyntaxNode,
    ) {
        let syntax_db = db.upcast();
        let green_node = node.green_node(syntax_db);
        match &green_node.details {
            syntax::node::green::GreenNodeDetails::Token(text) => {
                if green_node.kind == SyntaxKind::TokenNewline {
                    self.encoder.next_line();
                    return;
                }

                let width = text.len() as u32;
                let maybe_semantic_kind = self
                    .offset_to_kind_lookahead
                    .remove(&node.offset())
                    .or_else(|| SemanticTokenKind::from_syntax_node(db, file_id, node));
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
                let children = syntax_db.get_children(node.clone());
                match green_node.kind {
                    SyntaxKind::Param => {
                        self.mark_future_token(
                            ast::Param::from_syntax_node(syntax_db, node)
                                .name(syntax_db)
                                .as_syntax_node()
                                .offset(),
                            SemanticTokenKind::Parameter,
                        );
                    }
                    SyntaxKind::FunctionWithBody => {
                        self.mark_future_token(
                            ast::FunctionWithBody::from_syntax_node(syntax_db, node)
                                .declaration(syntax_db)
                                .name(syntax_db)
                                .as_syntax_node()
                                .offset(),
                            SemanticTokenKind::Function,
                        );
                    }
                    SyntaxKind::ItemStruct => self.mark_future_token(
                        ast::ItemStruct::from_syntax_node(syntax_db, node)
                            .name(syntax_db)
                            .as_syntax_node()
                            .offset(),
                        SemanticTokenKind::Struct,
                    ),
                    SyntaxKind::ItemEnum => self.mark_future_token(
                        ast::ItemEnum::from_syntax_node(syntax_db, node)
                            .name(syntax_db)
                            .as_syntax_node()
                            .offset(),
                        SemanticTokenKind::Enum,
                    ),
                    _ => {}
                }
                for child in children.iter() {
                    self.find_semantic_tokens(db, file_id, data, child.clone());
                }
            }
        }
    }

    fn mark_future_token(&mut self, offset: TextOffset, semantic_kind: SemanticTokenKind) {
        self.offset_to_kind_lookahead.insert(offset, semantic_kind);
    }
}
