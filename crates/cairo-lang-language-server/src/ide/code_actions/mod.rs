use cairo_lang_syntax::node::SyntaxNode;
use itertools::Itertools;
use lsp_types::{
    CodeAction, CodeActionOrCommand, CodeActionParams, CodeActionResponse, Diagnostic,
    NumberOrString,
};
use tracing::debug;

use crate::lang::db::{AnalysisDatabase, LsSyntaxGroup};
use crate::lang::lsp::{LsProtoGroup, ToCairo};

mod add_missing_trait;
mod expand_macro;
mod fill_struct_fields;
mod rename_unused_variable;

/// Compute commands for a given text document and range. These commands are typically code fixes to
/// either fix problems or to beautify/refactor code.
pub fn code_actions(params: CodeActionParams, db: &AnalysisDatabase) -> Option<CodeActionResponse> {
    let mut actions = Vec::with_capacity(params.context.diagnostics.len());
    let file_id = db.file_for_url(&params.text_document.uri)?;
    let node = db.find_syntax_node_at_position(file_id, params.range.start.to_cairo())?;

    actions.extend(
        get_code_actions_for_diagnostics(db, &node, &params)
            .into_iter()
            .map(CodeActionOrCommand::from),
    );

    actions.extend(
        expand_macro::expand_macro(db, node.clone()).into_iter().map(CodeActionOrCommand::from),
    );

    Some(actions)
}

/// Extracts [`Diagnostic`] code if it's given as a string, returns None otherwise.
fn resolve_code(diagnostic: &Diagnostic) -> Option<&str> {
    match &diagnostic.code {
        Some(NumberOrString::String(code)) => Some(code),
        Some(NumberOrString::Number(code)) => {
            debug!("diagnostic code is not a string: `{code}`");
            None
        }
        None => {
            debug!("diagnostic code is missing");
            None
        }
    }
}

/// Generate code actions for a given diagnostics.
///
/// # Arguments
///
/// * `db` - A reference to the Salsa database.
/// * `node` - The syntax node where the diagnostic is located.
/// * `diagnostics` - The diagnostics to generate code actions for.
/// * `params` - The parameters for the code action request.
///
/// # Returns
///
/// A vector of [`CodeAction`] objects that can be applied to resolve the diagnostics.
fn get_code_actions_for_diagnostics(
    db: &AnalysisDatabase,
    node: &SyntaxNode,
    params: &CodeActionParams,
) -> Vec<CodeAction> {
    params
        .context
        .diagnostics
        .iter()
        .filter_map(|diagnostic| Some((resolve_code(diagnostic)?, diagnostic)))
        .into_group_map()
        .into_iter()
        .flat_map(|(code, diagnostics)| match code {
            "E0001" => diagnostics
                .into_iter()
                .map(|diagnostic| {
                    rename_unused_variable::rename_unused_variable(
                        db,
                        node,
                        diagnostic.clone(),
                        params.text_document.uri.clone(),
                    )
                })
                .collect_vec(),
            "E0002" => {
                add_missing_trait::add_missing_trait(db, node, params.text_document.uri.clone())
            }
            "E0003" => fill_struct_fields::fill_struct_fields(db, node.clone(), params)
                .map(|result| vec![result])
                .unwrap_or_default(),
            _ => {
                debug!("no code actions for diagnostic code: {code}");
                vec![]
            }
        })
        .collect_vec()
}
