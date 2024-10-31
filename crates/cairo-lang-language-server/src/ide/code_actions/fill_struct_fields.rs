use std::collections::HashMap;

use cairo_lang_defs::ids::LanguageElementId;
use cairo_lang_semantic::Expr;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::function_with_body::SemanticExprLookup;
use cairo_lang_semantic::items::visibility::peek_visible_in;
use cairo_lang_semantic::lookup_item::LookupItemEx;
use cairo_lang_syntax::node::ast::{ExprStructCtorCall, StructArg};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use lsp_types::{CodeAction, CodeActionKind, CodeActionParams, Range, TextEdit, WorkspaceEdit};
use tracing::error;

use crate::lang::db::{AnalysisDatabase, LsSemanticGroup, LsSyntaxGroup};
use crate::lang::lsp::ToLsp;

/// Generates a completion adding all visible struct members that have not yet been specified
/// to the constructor call, filling their values with a placeholder unit type.
pub fn fill_struct_fields(
    db: &AnalysisDatabase,
    node: SyntaxNode,
    params: &CodeActionParams,
) -> Option<CodeAction> {
    let module_file_id = db.find_module_file_containing_node(&node)?;
    let module_id = module_file_id.0;
    let file_id = module_file_id.file_id(db).ok()?;
    let function_id = db.find_lookup_item(&node)?.function_with_body()?;

    let constructor = db.first_ancestor_of_kind(node, SyntaxKind::ExprStructCtorCall)?;
    let constructor_expr = ExprStructCtorCall::from_syntax_node(db, constructor.clone());

    let mut last_important_element = None;
    let mut has_trailing_comma = false;

    for node in constructor.descendants(db) {
        match node.kind(db) {
            SyntaxKind::TokenComma => {
                has_trailing_comma = true;
                last_important_element = Some(node)
            }
            SyntaxKind::StructArgSingle => {
                has_trailing_comma = false;
                last_important_element = Some(node)
            }
            // Don't complete any fields if initialization contains tail.
            SyntaxKind::StructArgTail => return None,
            _ => {}
        }
    }

    let code_prefix = String::from(if !has_trailing_comma && last_important_element.is_some() {
        ", "
    } else {
        " "
    });

    let struct_arguments = constructor_expr.arguments(db);
    let left_brace = struct_arguments.lbrace(db);
    let struct_arguments = struct_arguments.arguments(db).elements(db);

    let already_present_arguments = struct_arguments
        .iter()
        .map(|member| match member {
            StructArg::StructArgSingle(argument) => {
                argument.identifier(db).token(db).as_syntax_node().get_text_without_trivia(db)
            }
            StructArg::StructArgTail(_) => unreachable!(),
        })
        .collect::<Vec<_>>();

    let constructor_expr_id =
        db.lookup_expr_by_ptr(function_id, constructor_expr.stable_ptr().into()).ok()?;

    let constructor_semantic = match db.expr_semantic(function_id, constructor_expr_id) {
        Expr::StructCtor(semantic) => semantic,
        _ => {
            error!(
                "Semantic expression obtained from StructCtorCall doesn't refer to constructor."
            );
            return None;
        }
    };

    let concrete_struct_id = constructor_semantic.concrete_struct_id;
    let struct_parent_module_id = concrete_struct_id.struct_id(db).parent_module(db);

    let arguments_to_complete = db
        .concrete_struct_members(concrete_struct_id)
        .ok()?
        .iter()
        .filter_map(|(name, member)| {
            let name = name.to_string();

            if already_present_arguments.contains(&name) {
                None
            } else if peek_visible_in(db, member.visibility, struct_parent_module_id, module_id) {
                Some(format!("{name}: ()"))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    let code_to_insert = code_prefix + &arguments_to_complete.join(", ");

    let edit_start = last_important_element
        .unwrap_or(left_brace.as_syntax_node())
        .span_end_without_trivia(db)
        .position_in_file(db, file_id)?
        .to_lsp();

    let mut changes = HashMap::new();
    let url = params.text_document.uri.clone();
    let change = TextEdit { range: Range::new(edit_start, edit_start), new_text: code_to_insert };

    changes.insert(url, vec![change]);

    let edit = WorkspaceEdit::new(changes);

    Some(CodeAction {
        title: String::from("Fill struct fields"),
        kind: Some(CodeActionKind::QUICKFIX),
        edit: Some(edit),
        ..Default::default()
    })
}
