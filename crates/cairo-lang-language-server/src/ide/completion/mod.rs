use cairo_lang_filesystem::ids::FileId;
use cairo_lang_semantic::items::us::get_use_path_segments;
use cairo_lang_semantic::resolve::AsSegments;
use cairo_lang_syntax::node::ast::PathSegment;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode, ast};
use cairo_lang_utils::Upcast;
use completions::struct_constructor_completions;
use lsp_types::{CompletionParams, CompletionResponse, CompletionTriggerKind, Position};
use tracing::debug;

use self::completions::{colon_colon_completions, dot_completions, generic_completions};
use crate::lang::db::{AnalysisDatabase, LsSemanticGroup, LsSyntaxGroup};
use crate::lang::lsp::{LsProtoGroup, ToCairo};

mod completions;

/// Compute completion items at a given cursor position.
pub fn complete(params: CompletionParams, db: &AnalysisDatabase) -> Option<CompletionResponse> {
    let text_document_position = params.text_document_position;
    let file_id = db.file_for_url(&text_document_position.text_document.uri)?;
    let mut position = text_document_position.position;
    position.character = position.character.saturating_sub(1);

    let mut node = db.find_syntax_node_at_position(file_id, position.to_cairo())?;
    let lookup_items = db.collect_lookup_items_stack(&node)?;
    let module_file_id = db.find_module_file_containing_node(&node)?;

    // Skip trivia.
    while ast::Trivium::is_variant(node.kind(db))
        || node.kind(db) == SyntaxKind::Trivia
        || node.kind(db).is_token()
    {
        node = node.parent().unwrap_or(node);
    }

    let trigger_kind =
        params.context.map(|it| it.trigger_kind).unwrap_or(CompletionTriggerKind::INVOKED);

    match completion_kind(db, node, position, file_id) {
        CompletionKind::Dot(expr) => {
            dot_completions(db, file_id, lookup_items, expr).map(CompletionResponse::Array)
        }
        CompletionKind::ColonColon(segments) if !segments.is_empty() => {
            colon_colon_completions(db, module_file_id, lookup_items, segments)
                .map(CompletionResponse::Array)
        }
        CompletionKind::StructConstructor(constructor) => {
            struct_constructor_completions(db, lookup_items, constructor)
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
    StructConstructor(ast::ExprStructCtorCall),
}

fn completion_kind(
    db: &AnalysisDatabase,
    node: SyntaxNode,
    position: Position,
    file_id: FileId,
) -> CompletionKind {
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
            let Ok(mut segments) = get_use_path_segments(db.upcast(), use_ast) else {
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
                let Ok(mut segments) =
                    get_use_path_segments(db.upcast(), ast::UsePath::Leaf(use_ast))
                else {
                    debug!("Generic");
                    return CompletionKind::ColonColon(vec![]);
                };
                segments.pop();
                debug!("ColonColon");
                return CompletionKind::ColonColon(segments);
            }
        }
        SyntaxKind::TerminalLBrace | SyntaxKind::TerminalRBrace | SyntaxKind::TerminalComma => {
            if let Some(constructor_node) =
                db.first_ancestor_of_kind(node, SyntaxKind::ExprStructCtorCall)
            {
                return CompletionKind::StructConstructor(
                    ast::ExprStructCtorCall::from_syntax_node(db, constructor_node),
                );
            }
        }
        // Show completions only if struct tail is separated from the cursor by a newline.
        // Exclude cases like: `<cursor>..id`, `.<cursor>.id`, `..<cursor>id`
        SyntaxKind::TerminalDotDot => {
            let dot_dot_node_trivia = ast::TerminalDotDot::from_syntax_node(db, node.clone())
                .leading_trivia(db)
                .elements(db);

            let mut generate_completion = false;
            let mut node_found_in_trivia = false;

            if let Some(node_at_position) =
                db.find_syntax_node_at_position(file_id, position.to_cairo())
            {
                for trivium in dot_dot_node_trivia {
                    if trivium.as_syntax_node() == node_at_position {
                        node_found_in_trivia = true;
                    }

                    if node_found_in_trivia && matches!(trivium, ast::Trivium::Newline(_)) {
                        generate_completion = true;
                    }
                }
            }

            if generate_completion {
                if let Some(constructor_node) =
                    db.first_ancestor_of_kind(node, SyntaxKind::ExprStructCtorCall)
                {
                    return CompletionKind::StructConstructor(
                        ast::ExprStructCtorCall::from_syntax_node(db, constructor_node),
                    );
                }
            }
        }
        _ => (),
    }
    debug!("Generic");
    CompletionKind::ColonColon(vec![])
}

fn completion_kind_from_path_node(db: &AnalysisDatabase, parent: SyntaxNode) -> CompletionKind {
    debug!("completion_kind_from_path_node: {}", parent.clone().get_text_without_trivia(db));
    let expr = ast::ExprPath::from_syntax_node(db, parent);
    debug!("has_tail: {}", expr.has_tail(db));
    let mut segments = expr.to_segments(db);
    if expr.has_tail(db) {
        segments.pop();
    }
    CompletionKind::ColonColon(segments)
}
