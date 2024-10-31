use std::collections::HashMap;

use cairo_lang_defs::ids::{FunctionWithBodyId, LanguageElementId, ModuleId};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::function_with_body::SemanticExprLookup;
use cairo_lang_semantic::items::structure::concrete_struct_members;
use cairo_lang_semantic::items::visibility::peek_visible_in;
use cairo_lang_semantic::lookup_item::LookupItemEx;
use cairo_lang_semantic::{ConcreteStructId, Expr, ExprStructCtor};
use cairo_lang_syntax::node::ast::{
    ExprStructCtorCall, StructArg, TerminalLBrace, TerminalRBrace, Trivium,
};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use itertools::Itertools;
use lsp_types::{CodeAction, CodeActionKind, CodeActionParams, Range, TextEdit, WorkspaceEdit};

use crate::lang::db::{AnalysisDatabase, LsSemanticGroup, LsSyntaxGroup};
use crate::lang::lsp::ToLsp;

/// Generates a completion adding all visible struct members that have not yet been specified
/// to the constructor call, filling their values with a placeholder unit type.
pub fn fill_struct_members(
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
    let constructor_semantic = constructor_semantic(db, &constructor_expr, function_id)?;
    let concrete_struct_id = constructor_semantic.concrete_struct_id;

    let struct_arguments = constructor_expr.arguments(db);
    let left_brace = struct_arguments.lbrace(db);
    let right_brace = struct_arguments.rbrace(db);
    let struct_argumentss = struct_arguments.arguments(db).elements(db);

    let first_argument = struct_argumentss.first();

    // We will heuristically indent all subsequent arguments identically
    // to how the first of them is indented.
    let indentation = first_argument
        .map(|argument| whitespace_before_struct_argument(db, argument))
        .unwrap_or_default();

    let multiline = is_newline_between_braces(db, &left_brace, &right_brace);

    let edit_start = match first_argument {
        Some(arg) => arg.as_syntax_node().offset().position_in_file(db, file_id)?.to_lsp(),
        None => {
            let mut start =
                right_brace.as_syntax_node().offset().position_in_file(db, file_id)?.to_lsp();
            start.character += 1;
            start
        }
    };

    let edit_stop = right_brace.as_syntax_node().offset().position_in_file(db, file_id)?.to_lsp();

    let already_present_arguments =
        already_present_arguments_formatted(db, &struct_argumentss, &indentation)?;

    let arguments_to_complete = arguments_to_complete_formatted(
        db,
        concrete_struct_id,
        module_id,
        &already_present_arguments,
        &indentation,
    )?;

    let code_to_insert = already_present_arguments
        .values()
        .chain(arguments_to_complete.iter())
        .join(if multiline { ",\n" } else { ", " })
        .to_string();

    let mut changes = HashMap::new();
    let url = params.text_document.uri.clone();
    let change = TextEdit { range: Range::new(edit_start, edit_stop), new_text: code_to_insert };

    changes.insert(url, vec![change]);

    let edit = WorkspaceEdit::new(changes);

    Some(CodeAction {
        title: String::from("Fill struct fields"),
        kind: Some(CodeActionKind::QUICKFIX),
        edit: Some(edit),
        ..Default::default()
    })
}

/// Retreives semantic counterpart of [`ExprCtorCall`] in context of [`FunctionWithBodyId`].
fn constructor_semantic(
    db: &AnalysisDatabase,
    constructor: &ExprStructCtorCall,
    function_id: FunctionWithBodyId,
) -> Option<ExprStructCtor> {
    let constructor_expr_id =
        db.lookup_expr_by_ptr(function_id, constructor.stable_ptr().into()).ok()?;

    match db.expr_semantic(function_id, constructor_expr_id) {
        Expr::StructCtor(semantic) => Some(semantic),
        _ => None,
    }
}

/// Discovers concrete members of a struct pointed to by [`ConcreteStructId`],
/// chooses those of them which doesn't appear in `already_present_args`
/// and formats them with `indentation`.
fn arguments_to_complete_formatted(
    db: &AnalysisDatabase,
    concrete_struct_id: ConcreteStructId,
    current_module_id: ModuleId,
    already_present_args: &HashMap<String, String>,
    indentation: &str,
) -> Option<Vec<String>> {
    let struct_parent_module_id = concrete_struct_id.struct_id(db).parent_module(db);

    let args = concrete_struct_members(db, concrete_struct_id)
        .ok()?
        .iter()
        .filter_map(|(name, member)| {
            let name = name.to_string();

            if already_present_args.contains_key(&name) {
                None
            } else if peek_visible_in(
                db,
                member.visibility,
                struct_parent_module_id,
                current_module_id,
            ) {
                Some(format!("{indentation}{name}: ()"))
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    Some(args)
}

/// Checks for newline between [`TerminalLBrace`] and [`TerminalRBrace`].
fn is_newline_between_braces(
    db: &AnalysisDatabase,
    left_brace: &TerminalLBrace,
    right_brace: &TerminalRBrace,
) -> bool {
    left_brace
        .trailing_trivia(db)
        .elements(db)
        .iter()
        .chain(right_brace.leading_trivia(db).elements(db).iter())
        .any(|trivium| matches!(trivium, Trivium::Newline(_)))
}

/// Gets all whitespace placed before [`StructArg`] in line it appears in
/// and joins it into one string.
fn whitespace_before_struct_argument(db: &AnalysisDatabase, argument: &StructArg) -> String {
    if let StructArg::StructArgSingle(argument) = argument {
        argument
            .identifier(db)
            .leading_trivia(db)
            .elements(db)
            .iter()
            .rev()
            .map_while(|trivium| match trivium {
                Trivium::Newline(_) => None,
                Trivium::Whitespace(whitespace) => Some(whitespace.as_syntax_node().get_text(db)),
                _ => Some(String::new()),
            })
            .join("")
    } else {
        String::new()
    }
}

/// Constructs a mapping (argument name -> member constructor entry) based on [`StructArg`]s,
/// formatted with `indentation`.
fn already_present_arguments_formatted(
    db: &AnalysisDatabase,
    struct_arguments: &[StructArg],
    indentation: &str,
) -> Option<HashMap<String, String>> {
    struct_arguments
        .iter()
        .map(|member| match member {
            StructArg::StructArgSingle(argument) => {
                let name =
                    argument.identifier(db).token(db).as_syntax_node().get_text_without_trivia(db);
                let value = argument.arg_expr(db).as_syntax_node().get_text_without_trivia(db);

                Some((name.clone(), format!("{indentation}{name}{value}")))
            }
            // We don't complete anything if tail is present.
            StructArg::StructArgTail(_) => None,
        })
        .collect::<Option<HashMap<_, _>>>()
}
