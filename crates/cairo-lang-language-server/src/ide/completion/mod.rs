use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_defs::ids::{FileIndex, ModuleFileId};
use cairo_lang_semantic::items::us::get_use_segments;
use cairo_lang_semantic::resolve::AsSegments;
use cairo_lang_syntax::node::ast::PathSegment;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::Upcast;
use tower_lsp::lsp_types::{CompletionParams, CompletionResponse, CompletionTriggerKind};
use tracing::{debug, error};

use self::completions::{colon_colon_completions, dot_completions, generic_completions};
use crate::lang::lsp::LsProtoGroup;
use crate::{find_node_module, get_node_and_lookup_items};

mod completions;

/// Compute completion items at a given cursor position.
#[tracing::instrument(
    level = "debug",
    skip_all,
    fields(uri = %params.text_document_position.text_document.uri)
)]
pub fn complete(params: CompletionParams, db: &RootDatabase) -> Option<CompletionResponse> {
    let text_document_position = params.text_document_position;
    let file_id = db.file_for_url(&text_document_position.text_document.uri);
    let mut position = text_document_position.position;
    position.character = position.character.saturating_sub(1);

    let (mut node, lookup_items) = get_node_and_lookup_items(db, file_id, position)?;

    // Find module.
    let Some(module_id) = find_node_module(db, file_id, node.clone()) else {
        error!("completion failed: failed to find module");
        return None;
    };
    let file_index = FileIndex(0);
    let module_file_id = ModuleFileId(module_id, file_index);

    // Skip trivia.
    while ast::Trivium::is_variant(node.kind(db))
        || node.kind(db) == SyntaxKind::Trivia
        || node.kind(db).is_token()
    {
        node = node.parent().unwrap_or(node);
    }

    let trigger_kind =
        params.context.map(|it| it.trigger_kind).unwrap_or(CompletionTriggerKind::INVOKED);

    match completion_kind(db, node) {
        CompletionKind::Dot(expr) => {
            dot_completions(db, file_id, lookup_items, expr).map(CompletionResponse::Array)
        }
        CompletionKind::ColonColon(segments) if !segments.is_empty() => {
            colon_colon_completions(db, module_file_id, lookup_items, segments)
                .map(CompletionResponse::Array)
        }
        _ if trigger_kind == CompletionTriggerKind::INVOKED => {
            Some(CompletionResponse::Array(generic_completions(db, module_file_id, lookup_items)))
        }
        _ => None,
    }
}

enum CompletionKind {
    Dot(ast::ExprBinary),
    ColonColon(Vec<PathSegment>),
}

#[tracing::instrument(level = "trace", skip_all)]
fn completion_kind(db: &RootDatabase, node: SyntaxNode) -> CompletionKind {
    debug!("node.kind: {:#?}", node.kind(db));
    match node.kind(db) {
        SyntaxKind::TerminalDot => {
            let parent = node.parent().unwrap();
            if parent.kind(db) == SyntaxKind::ExprBinary {
                return CompletionKind::Dot(ast::ExprBinary::from_syntax_node(db, parent));
            }
        }
        SyntaxKind::TerminalColonColon => {
            let parent = node.parent().unwrap();
            debug!("parent.kind: {:#?}", parent.kind(db));
            if parent.kind(db) == SyntaxKind::ExprPath {
                return completion_kind_from_path_node(db, parent);
            }
            let grandparent = parent.parent().unwrap();
            debug!("grandparent.kind: {:#?}", grandparent.kind(db));
            if grandparent.kind(db) == SyntaxKind::ExprPath {
                return completion_kind_from_path_node(db, grandparent);
            }
            let (use_ast, should_pop) = if parent.kind(db) == SyntaxKind::UsePathLeaf {
                (ast::UsePath::Leaf(ast::UsePathLeaf::from_syntax_node(db, parent)), true)
            } else if grandparent.kind(db) == SyntaxKind::UsePathLeaf {
                (ast::UsePath::Leaf(ast::UsePathLeaf::from_syntax_node(db, grandparent)), true)
            } else if parent.kind(db) == SyntaxKind::UsePathSingle {
                (ast::UsePath::Single(ast::UsePathSingle::from_syntax_node(db, parent)), false)
            } else if grandparent.kind(db) == SyntaxKind::UsePathSingle {
                (ast::UsePath::Single(ast::UsePathSingle::from_syntax_node(db, grandparent)), false)
            } else {
                debug!("Generic");
                return CompletionKind::ColonColon(vec![]);
            };
            let mut segments = vec![];
            let Ok(()) = get_use_segments(db.upcast(), &use_ast, &mut segments) else {
                debug!("Generic");
                return CompletionKind::ColonColon(vec![]);
            };
            if should_pop {
                segments.pop();
            }
            debug!("ColonColon");
            return CompletionKind::ColonColon(segments);
        }
        SyntaxKind::TerminalIdentifier => {
            let parent = node.parent().unwrap();
            debug!("parent.kind: {:#?}", parent.kind(db));
            let grandparent = parent.parent().unwrap();
            debug!("grandparent.kind: {:#?}", grandparent.kind(db));
            if grandparent.kind(db) == SyntaxKind::ExprPath {
                if db.get_children(grandparent.clone())[0].stable_ptr() != parent.stable_ptr() {
                    // Not the first segment.
                    debug!("Not first segment");
                    return completion_kind_from_path_node(db, grandparent);
                }
                // First segment.
                let grandgrandparent = grandparent.parent().unwrap();
                debug!("grandgrandparent.kind: {:#?}", grandgrandparent.kind(db));
                if grandgrandparent.kind(db) == SyntaxKind::ExprBinary {
                    let expr = ast::ExprBinary::from_syntax_node(db, grandgrandparent.clone());
                    if matches!(
                        ast::ExprBinary::from_syntax_node(db, grandgrandparent).op(db),
                        ast::BinaryOperator::Dot(_)
                    ) {
                        debug!("Dot");
                        return CompletionKind::Dot(expr);
                    }
                }
            }
            if grandparent.kind(db) == SyntaxKind::UsePathLeaf {
                let use_ast = ast::UsePathLeaf::from_syntax_node(db, grandparent);
                let mut segments = vec![];
                let Ok(()) =
                    get_use_segments(db.upcast(), &ast::UsePath::Leaf(use_ast), &mut segments)
                else {
                    debug!("Generic");
                    return CompletionKind::ColonColon(vec![]);
                };
                segments.pop();
                debug!("ColonColon");
                return CompletionKind::ColonColon(segments);
            }
        }
        _ => (),
    }
    debug!("Generic");
    CompletionKind::ColonColon(vec![])
}

#[tracing::instrument(level = "trace", skip_all)]
fn completion_kind_from_path_node(db: &RootDatabase, parent: SyntaxNode) -> CompletionKind {
    debug!("completion_kind_from_path_node: {}", parent.clone().get_text_without_trivia(db));
    let expr = ast::ExprPath::from_syntax_node(db, parent);
    debug!("has_tail: {}", expr.has_tail(db));
    let mut segments = expr.to_segments(db);
    if expr.has_tail(db) {
        segments.pop();
    }
    CompletionKind::ColonColon(segments)
}
