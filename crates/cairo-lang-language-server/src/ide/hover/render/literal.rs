use cairo_lang_defs::ids::{ConstantLongId, FunctionWithBodyId};
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::function_with_body::SemanticExprLookup;
use cairo_lang_semantic::lookup_item::LookupItemEx;
use cairo_lang_syntax::node::ast::{
    Expr, ItemConstant, TerminalLiteralNumber, TerminalShortString, TerminalString,
};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::{Intern, Upcast};
use indoc::formatdoc;
use lsp_types::Hover;

use crate::ide::hover::markdown_contents;
use crate::lang::db::{AnalysisDatabase, LsSemanticGroup, LsSyntaxGroup};
use crate::lang::lsp::ToLsp;

/// Narrows down [`SyntaxNode`] to [`TerminalLiteralNumber`], [`TerminalString`] or
/// [`TerminalShortString`] if it represents some literal
/// and renders a hover containing its value and type, returns None otherwise.
#[tracing::instrument(level = "trace", skip_all)]
pub fn literal(db: &AnalysisDatabase, node: &SyntaxNode, file_id: FileId) -> Option<Hover> {
    match node.kind(db) {
        SyntaxKind::TokenLiteralNumber => {
            let parent = node.parent()?;
            let literal = TerminalLiteralNumber::from_syntax_node(db, parent.clone());
            let ty = find_type(db, parent)?;
            number_hover(db, &literal, &ty, file_id)
        }
        SyntaxKind::TokenString => {
            let parent = node.parent()?;
            let literal = TerminalString::from_syntax_node(db, parent.clone());
            let ty = find_type(db, parent)?;
            string_hover(db, &literal, &ty, file_id)
        }
        SyntaxKind::TokenShortString => {
            let parent = node.parent()?;
            let literal = TerminalShortString::from_syntax_node(db, parent.clone());
            let ty = find_type(db, parent)?;
            short_string_hover(db, &literal, &ty, file_id)
        }
        _ => None,
    }
}

/// Gets the type of an expression associated with [`SyntaxNode`].
fn find_type(db: &AnalysisDatabase, node: SyntaxNode) -> Option<String> {
    if let Some(function_id) = db.find_lookup_item(&node)?.function_with_body() {
        find_type_in_function_context(db, node.clone(), function_id)
    } else {
        find_type_in_const_declaration(db, node)
    }
}

/// Gets the type of an expression associated with [`SyntaxNode`] assuming it's defined in the
/// context of function.
fn find_type_in_function_context(
    db: &AnalysisDatabase,
    node: SyntaxNode,
    function_id: FunctionWithBodyId,
) -> Option<String> {
    let expr = Expr::from_syntax_node(db, node);
    let expr_id = db.lookup_expr_by_ptr(function_id, expr.stable_ptr()).ok()?;
    Some(db.expr_semantic(function_id, expr_id).ty().format(db))
}

/// Gets the type of an expression associated with [`SyntaxNode`] assuming it's a const item.
fn find_type_in_const_declaration(db: &AnalysisDatabase, node: SyntaxNode) -> Option<String> {
    let module_file_id = db.find_module_file_containing_node(&node)?;

    let const_node = db.first_ancestor_of_kind(node, SyntaxKind::ItemConstant)?;
    let const_item = ItemConstant::from_syntax_node(db, const_node);
    let const_item_id = ConstantLongId(module_file_id, const_item.stable_ptr()).intern(db);

    Some(db.constant_const_type(const_item_id).ok()?.format(db))
}

/// Formats the number literal writing its decimal, hexadecimal and binary value and type.
fn number_hover(
    db: &AnalysisDatabase,
    literal: &TerminalLiteralNumber,
    ty: &str,
    file_id: FileId,
) -> Option<Hover> {
    let value = literal.numeric_value(db)?;

    let representation = formatdoc!(
        "
        ```cairo
        {ty}
        ```
        ---
        value of literal: `{value} ({value:#x} | {value:#b})`
        "
    );

    Some(Hover {
        contents: markdown_contents(representation),
        range: literal
            .as_syntax_node()
            .span_without_trivia(db.upcast())
            .position_in_file(db.upcast(), file_id)
            .map(|position| position.to_lsp()),
    })
}

/// Formats the number literal writing it along with the `core::byte_array::ByteArray` type.
fn string_hover(
    db: &AnalysisDatabase,
    literal: &TerminalString,
    ty: &str,
    file_id: FileId,
) -> Option<Hover> {
    let representation = formatdoc!(
        r#"
        ```cairo
        {ty}
        ```
        "#
    );

    Some(Hover {
        contents: markdown_contents(representation),
        range: literal
            .as_syntax_node()
            .span_without_trivia(db.upcast())
            .position_in_file(db.upcast(), file_id)
            .map(|position| position.to_lsp()),
    })
}

/// Formats the short string literal writing its textual and numeric value along with the
/// `core::felt252` type.
fn short_string_hover(
    db: &AnalysisDatabase,
    literal: &TerminalShortString,
    ty: &str,
    file_id: FileId,
) -> Option<Hover> {
    let representation = match (literal.numeric_value(db), literal.string_value(db)) {
        (None, _) => None,
        (Some(numeric), None) => Some(formatdoc!(
            "
            ```cairo
            {ty}
            ```
            ---
            value of literal: `{numeric:#x}`
            "
        )),
        (Some(numeric), Some(string)) => Some(formatdoc!(
            "
            ```cairo
            {ty}
            ```
            ---
            value of literal: `'{string}' ({numeric:#x})`
            "
        )),
    }?;

    Some(Hover {
        contents: markdown_contents(representation),
        range: literal
            .as_syntax_node()
            .span_without_trivia(db.upcast())
            .position_in_file(db.upcast(), file_id)
            .map(|position| position.to_lsp()),
    })
}
