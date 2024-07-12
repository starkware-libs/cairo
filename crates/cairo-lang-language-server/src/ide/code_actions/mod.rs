use cairo_lang_syntax::node::SyntaxNode;
use tower_lsp::lsp_types::{
    CodeAction, CodeActionOrCommand, CodeActionParams, CodeActionResponse, Diagnostic,
    NumberOrString,
};
use tracing::debug;

use crate::lang::db::{AnalysisDatabase, LsSyntaxGroup};
use crate::lang::lsp::{LsProtoGroup, ToCairo};

mod expand_macro;
mod rename_unused_variable;

/// Compute commands for a given text document and range. These commands are typically code fixes to
/// either fix problems or to beautify/refactor code.
#[tracing::instrument(
    level = "debug",
    skip_all,
    fields(uri = %params.text_document.uri)
)]
pub fn code_actions(params: CodeActionParams, db: &AnalysisDatabase) -> Option<CodeActionResponse> {
    let mut actions = Vec::with_capacity(params.context.diagnostics.len());
    let file_id = db.file_for_url(&params.text_document.uri)?;
    let node = db.find_syntax_node_at_position(file_id, params.range.start.to_cairo())?;
    for diagnostic in params.context.diagnostics.iter() {
        actions.extend(
            get_code_actions_for_diagnostic(db, &node, diagnostic, &params)
                .into_iter()
                .map(CodeActionOrCommand::from),
        );
    }
    Some(actions)
}

/// Generate code actions for a given diagnostic.
///
/// # Arguments
///
/// * `db` - A reference to the Salsa database.
/// * `node` - The syntax node where the diagnostic is located.
/// * `diagnostic` - The diagnostic for which to generate code actions.
/// * `params` - The parameters for the code action request.
///
/// # Returns
///
/// A vector of [`CodeAction`] objects that can be applied to resolve the diagnostic.
fn get_code_actions_for_diagnostic(
    db: &AnalysisDatabase,
    node: &SyntaxNode,
    diagnostic: &Diagnostic,
    params: &CodeActionParams,
) -> Vec<CodeAction> {
    let mut result = vec![];

    for_err_code(db, node, diagnostic, params, &mut result);
    expand_macro::expand_macro(db, node.clone(), &mut result);

    result
}

fn for_err_code(
    db: &AnalysisDatabase,
    node: &SyntaxNode,
    diagnostic: &Diagnostic,
    params: &CodeActionParams,
    result: &mut Vec<CodeAction>,
) {
    let code = match &diagnostic.code {
        Some(NumberOrString::String(code)) => Some(code.as_str()),
        Some(NumberOrString::Number(code)) => {
            debug!("diagnostic code is not a string: `{code}`");
            None
        }
        None => {
            debug!("diagnostic code is missing");
            None
        }
    };

    if let Some(code) = code {
        match code {
            "E0001" => result.push(rename_unused_variable::rename_unused_variable(
                db,
                node,
                diagnostic.clone(),
                params.text_document.uri.clone(),
            )),
            code => {
                debug!("no code actions for diagnostic code: {code}");
            }
        };
    }
}
