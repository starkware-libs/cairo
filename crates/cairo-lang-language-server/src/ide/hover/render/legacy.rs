use cairo_lang_defs::ids::{FunctionWithBodyId, LookupItemId};
use cairo_lang_diagnostics::ToOption;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::function_with_body::SemanticExprLookup;
use cairo_lang_semantic::lookup_item::LookupItemEx;
use cairo_lang_semantic::Mutability;
use cairo_lang_syntax::node::ast::{Expr, Pattern, TerminalIdentifier};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::Upcast;
use tower_lsp::lsp_types::Hover;

use crate::ide::hover::markdown_contents;
use crate::ide::hover::render::markdown::{fenced_code_block, RULE};
use crate::lang::db::{AnalysisDatabase, LsSemanticGroup};

/// Legacy hover rendering backported from Cairo 2.6.3 codebase.
///
/// This logic is meant for gradual replacement with new-style hovers and eventually be removed.
pub fn legacy(db: &AnalysisDatabase, identifier: &TerminalIdentifier) -> Option<Hover> {
    let node = identifier.as_syntax_node();
    let lookup_item_id = db.find_lookup_item(&node)?;
    let function_id = lookup_item_id.function_with_body()?;

    // Build texts.
    let mut hints = Vec::new();
    if let Some(hint) = get_pattern_hint(db, function_id, node.clone()) {
        hints.push(hint);
    } else if let Some(hint) = get_expr_hint(db, function_id, node.clone()) {
        hints.push(hint);
    };
    if let Some(hint) = get_identifier_hint(db, lookup_item_id, node) {
        hints.push(hint);
    };

    let hints = hints.join(RULE);
    Some(Hover { contents: markdown_contents(hints), range: None })
}

/// If the node is an identifier, retrieves a hover hint for it.
fn get_identifier_hint(
    db: &AnalysisDatabase,
    lookup_item_id: LookupItemId,
    node: SyntaxNode,
) -> Option<String> {
    let syntax_db = db.upcast();
    if node.kind(syntax_db) != SyntaxKind::TokenIdentifier {
        return None;
    }
    let identifier = TerminalIdentifier::from_syntax_node(syntax_db, node.parent().unwrap());
    let item = db.lookup_resolved_generic_item_by_ptr(lookup_item_id, identifier.stable_ptr())?;

    // TODO(spapini): Also include concrete item hints.
    // TODO(spapini): Format this better.
    Some(format!("`{}`", item.full_path(db)))
}

/// If the node is an expression, retrieves a hover hint for it.
fn get_expr_hint(
    db: &AnalysisDatabase,
    function_id: FunctionWithBodyId,
    node: SyntaxNode,
) -> Option<String> {
    let semantic_expr = nearest_semantic_expr(db, node, function_id)?;
    let text = match semantic_expr {
        cairo_lang_semantic::Expr::FunctionCall(call) => {
            let args = if let Ok(signature) =
                call.function.get_concrete(db).generic_function.generic_signature(db.upcast())
            {
                signature
                    .params
                    .iter()
                    .map(|arg| {
                        let mutability = match arg.mutability {
                            Mutability::Immutable => "",
                            Mutability::Mutable => "mut ",
                            Mutability::Reference => "ref ",
                        };
                        format!("{mutability}{}: {}", arg.name, arg.ty.format(db.upcast()))
                    })
                    .collect::<Vec<String>>()
                    .join(", ")
            } else {
                "".to_owned()
            };
            let mut s = format!(
                "fn {}({}) -> {}",
                call.function.name(db.upcast()),
                args,
                call.ty.format(db.upcast())
            );
            s.retain(|c| c != '"');
            s
        }
        _ => semantic_expr.ty().format(db),
    };
    // Format the hover text.
    Some(fenced_code_block(&text))
}

/// Returns the semantic expression for the current node.
fn nearest_semantic_expr(
    db: &AnalysisDatabase,
    mut node: SyntaxNode,
    function_id: FunctionWithBodyId,
) -> Option<cairo_lang_semantic::Expr> {
    loop {
        let syntax_db = db.upcast();
        if Expr::is_variant(node.kind(syntax_db)) {
            let expr_node = Expr::from_syntax_node(syntax_db, node.clone());
            if let Some(expr_id) =
                db.lookup_expr_by_ptr(function_id, expr_node.stable_ptr()).to_option()
            {
                let semantic_expr = db.expr_semantic(function_id, expr_id);
                return Some(semantic_expr);
            }
        }
        node = node.parent()?;
    }
}

/// If the node is a pattern, retrieves a hover hint for it.
fn get_pattern_hint(
    db: &AnalysisDatabase,
    function_id: FunctionWithBodyId,
    node: SyntaxNode,
) -> Option<String> {
    let semantic_pattern = nearest_semantic_pat(db, node, function_id)?;
    // Format the hover text.
    Some(format!("Type: `{}`", semantic_pattern.ty().format(db)))
}

/// Returns the semantic pattern for the current node.
fn nearest_semantic_pat(
    db: &AnalysisDatabase,
    mut node: SyntaxNode,
    function_id: FunctionWithBodyId,
) -> Option<cairo_lang_semantic::Pattern> {
    loop {
        let syntax_db = db.upcast();
        if Pattern::is_variant(node.kind(syntax_db)) {
            let pattern_node = Pattern::from_syntax_node(syntax_db, node.clone());
            if let Some(pattern_id) =
                db.lookup_pattern_by_ptr(function_id, pattern_node.stable_ptr()).to_option()
            {
                let semantic_pattern = db.pattern_semantic(function_id, pattern_id);
                return Some(semantic_pattern);
            }
        }
        node = node.parent()?;
    }
}
