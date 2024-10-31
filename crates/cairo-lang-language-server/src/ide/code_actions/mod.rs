use cairo_lang_syntax::node::SyntaxNode;
use cairo_lang_utils::ordered_hash_map::{Entry, OrderedHashMap};
use itertools::Itertools;
use lsp_types::{
    CodeAction, CodeActionOrCommand, CodeActionParams, CodeActionResponse, Diagnostic,
    NumberOrString,
};
use tracing::{debug, warn};

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

/// Generate code actions for a given diagnostics in context of [`CodeActionParams`].
///
/// # Arguments
///
/// * `db` - A reference to the Salsa database.
/// * `node` - The syntax node where the diagnostic is located.
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
    let mut diagnostic_groups_by_codes: OrderedHashMap<String, Vec<&Diagnostic>> =
        OrderedHashMap::default();

    for diagnostic in params.context.diagnostics.iter() {
        if let Some(code) = extract_code(diagnostic) {
            match diagnostic_groups_by_codes.entry(code.to_owned()) {
                Entry::Occupied(mut entry) => {
                    entry.get_mut().push(diagnostic);
                }
                Entry::Vacant(entry) => {
                    entry.insert(vec![diagnostic]);
                }
            }
        }
    }

    diagnostic_groups_by_codes
        .into_iter()
        .flat_map(|(code, diagnostics)| match code.as_str() {
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

/// Extracts [`Diagnostic`] code if it's given as a string, returns None otherwise.
fn extract_code(diagnostic: &Diagnostic) -> Option<&str> {
    match &diagnostic.code {
        Some(NumberOrString::String(code)) => Some(code),
        Some(NumberOrString::Number(code)) => {
            warn!("diagnostic code is not a string: `{code}`");
            None
        }
        None => None,
    }
}
